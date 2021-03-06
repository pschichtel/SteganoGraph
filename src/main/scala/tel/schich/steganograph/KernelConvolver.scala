package tel.schich.steganograph

import java.awt.Color.HSBtoRGB
import java.awt.image.BufferedImage

import scala.annotation.tailrec
import scala.math.Numeric.Implicits.infixNumericOps

object KernelConvolver {

    def ignore(pixels: Array[ARGB], width: Int, height: Int, x: Int, y: Int): ARGB = ARGB.zero

    def wrap(pixels: Array[ARGB], width: Int, height: Int, x: Int, y: Int): ARGB =
        pixels((y % height) * width + (x % width))

    def mirror(pixels: Array[ARGB], width: Int, height: Int, x: Int, y: Int): ARGB = {
        def edgeMirror(n: Int, max: Int) =
            if (n < 0) n.abs % max
            else if (n >= max) (max - 1) - (n % max)
            else n

        pixels(edgeMirror(y, height) * width + edgeMirror(x, width))
    }

    def extend(pixels: Array[ARGB], width: Int, height: Int, x: Int, y: Int): ARGB = {
        def snap(n: Int, max: Int) =
            if (n < 0) 0
            else if (n >= max) max - 1
            else n

        pixels(snap(y, height) * width + snap(x, width))
    }

    type Filter = (Int, Int, Array[ARGB], Continuation) => Array[ARGB]
    type Continuation = (Array[ARGB], Int, Int, Int, Int) => ARGB

    def main(args: Array[String]): Unit = {
        val filters: Array[(Filter, Continuation)] = args.drop(2).map(_.trim.toLowerCase).map { filterSpec =>
            val (filterName, contName) = filterSpec.split(':') match {
                case Array(f, c) => (f, c)
                case Array(f) => (f, "ignore")
            }
            val filter: Filter = filterName match {
                case "dither" => dither(closest(Seq(ARGB.zero, ARGB.A, ARGB.R, ARGB.G, ARGB.B, ARGB.R + ARGB.G, ARGB.R + ARGB.B, ARGB.G + ARGB.B, ARGB.one)))
                case "alpha" => perPixel(multiply(ARGB.A))
                case "red" => perPixel(multiply(ARGB.R))
                case "green" => perPixel(multiply(ARGB.G))
                case "blue" => perPixel(multiply(ARGB.B))
                case "red_green" => perPixel(multiply(ARGB.R + ARGB.G))
                case "red_blue" => perPixel(multiply(ARGB.R + ARGB.B))
                case "green_blue" => perPixel(multiply(ARGB.G + ARGB.B))
                case "grayscale" => perPixel(grayscale)
                case "sepia" => perPixel(sepia)
                case "noalpha" => perPixel(a => ARGB(1, a.r, a.g, a.b))
                case "sobel" => sobel
                case "hysteresis_sobel" => hysteresisSobel(0.005, 0.2)
                case "identity" => kernel(
                    0, 0, 0,
                    0, 1, 0,
                    0, 0, 0
                )
                case "mean3" => kernel(
                    1, 1, 1,
                    1, 1, 1,
                    1, 1, 1
                )
                case "mean" => kernel(
                    1, 1, 1, 1, 1,
                    1, 1, 1, 1, 1,
                    1, 1, 1, 1, 1,
                    1, 1, 1, 1, 1,
                    1, 1, 1, 1, 1
                )
                case "emboss" => kernel(
                    -2, -1, 0,
                    -1, 1, 1,
                    0, 1, 2
                )
                case "gauss3" => kernel(
                    1, 2, 1,
                    2, 4, 2,
                    1, 2, 1
                )
                case "gauss" => kernel(
                    1, 4, 6, 4, 1,
                    1, 16, 24, 16, 4,
                    1, 24, 36, 24, 6,
                    1, 16, 24, 16, 4,
                    1, 4, 6, 4, 1
                )
                case "edge" => kernel(
                    -1, -1, -1,
                    -1, 8, -1,
                    -1, -1, -1
                )
                case "edge_x" => kernel(
                    -1, 0, 1,
                    -1, 0, 1,
                    -1, 0, 1
                )
                case "edge_y" => kernel(
                    -1, -1, -1,
                    0, 0, 0,
                    1, 1, 1
                )
                case "sharpen" => kernel(
                    0, -1, 0,
                    -1, 5, -1,
                    0, -1, 0
                )
                case "laplace" => kernel(
                    0, -1, 0
                    -1, 4, -1,
                    0, -1, 0
                )
                case "laplace_diagonal" => kernel(
                    -1, -1, -1
                    -1, 8, -1,
                    -1, -1, -1
                )
                case "laplace_diagonal5" => kernel(
                    -1, -1, -1, -1, -1
                    -1, -1, 24, -1, -1,
                    -1, -1, -1, -1, -1
                )
                case _ => throw new IllegalArgumentException(s"Filter not found: $filterName")
            }

            val cont: Continuation = contName match {
                case "ignore" => ignore
                case "mirror" => mirror
                case "extend" => extend
                case _ => throw new IllegalArgumentException(s"Continuation not found: $contName")
            }

            (filter, cont)
        }


        val img = readImg(args(0))
        val width = img.getWidth
        val height = img.getHeight
        val inputPixels = (for {
            y <- 0 until height
            x <- 0 until width
        } yield extractChannels(img.getRGB(x, y))).toArray

        val processed = filters.foldLeft(inputPixels) {
            case (pixels, (filter, cont)) => filter(width, height, pixels, cont)
        }

        val output = new BufferedImage(width, height, img.getType)
        for {
            imgY <- 0 until height
            imgX <- 0 until width
        } {
            output.setRGB(imgX, imgY, processed(imgY * width + imgX).toInt)
        }

        writeImg(output, args(1))
    }


    def grayscale(p: ARGB): ARGB = {
        val avg = (p.r + p.g + p.b) / 3.0
        ARGB(p.a, avg, avg, avg)
    }

    def sepia(p: ARGB): ARGB = ARGB(
        p.a,
        math.min(1, (p.r * .393) + (p.g *.769) + (p.b * .189)),
        math.min(1, (p.r * .349) + (p.g *.686) + (p.b * .168)),
        math.min(1, (p.r * .272) + (p.g *.534) + (p.b * .131))
    )

    def perPixel(mapColor: ARGB => ARGB)(width: Int, height: Int, pixels: Array[ARGB], cont: Continuation): Array[ARGB] =
        pixels.map(mapColor)

    def dither(mapColor: ARGB => ARGB)(width: Int, height: Int, pixels: Array[ARGB], cont: Continuation): Array[ARGB] = {
        val out = Array.ofDim[ARGB](pixels.length)
        Array.copy(pixels, 0, out, 0, pixels.length)

        def idx(x: Int, y: Int) = y * width + x

        def update(x: Int, y: Int, err: ARGB): Unit = {
            val i = idx(x, y)
            if (out.isDefinedAt(i)) {
                out(i) = out(i) + err
            }
        }

        for {
            y <- 0 until height
            x <- 0 until width
        } yield {
            val i = idx(x, y)
            val pixel = out(i)
            val newPixel = mapColor(pixel)
            val err = pixel - newPixel

            out(i) = newPixel
            update(x + 1, y    , err * (7.0/16.0))
            update(x - 1, y + 1, err * (3.0/16.0))
            update(x    , y + 1, err * (5.0/16.0))
            update(x + 1, y + 1, err * (1.0/16.0))
        }

        out
    }

    def closest(palette: Seq[ARGB])(search: ARGB): ARGB =
        palette.minBy(_.distanceSquared(search))(Ordering.Double.TotalOrdering)

    def multiply(c: ARGB)(p: ARGB): ARGB = p * c

    def kernel(kernel: Double*)(width: Int, height: Int, pixels: Array[ARGB], cont: Continuation): Array[ARGB] =
        applyKernel(width, height, pixels, cont, kernel.toArray)

    def pixel(pixels: Array[ARGB], cont: Continuation, width: Int, height: Int)(x: Int, y: Int): ARGB =
        if (x >= 0 && x < width && y >= 0 && y < height) pixels(y * width + x)
        else cont(pixels, width, height, x, y)

    def applyKernel(width: Int, height: Int, pixels: Array[ARGB], cont: Continuation, kernel: Array[Double]): Array[ARGB] = {
        val kernelSize = math.sqrt(kernel.length).toInt
        val kernelRadius = kernelSize / 2 // integer division to round down
        val kernelOffsets = (for {
            x <- 0 until kernelSize
            y <- 0 until kernelSize
        } yield {
            (kernel(y * kernelSize + x), x - kernelRadius, y - kernelRadius)
        }).toArray

        val pix = pixel(pixels, cont, width, height) _
        val processed = (for {
            imgY <- 0 until height
            imgX <- 0 until width
        } yield {
            (for ((w, x, y) <- kernelOffsets) yield {
                val actualX = imgX + x
                val actualY = imgY + y

                pix(actualX, actualY) * w
            }).sum
        }).toArray

        normalizeScale(processed)
    }

    def normalizeScale(pixels: Array[ARGB]): Array[ARGB] = {
        val min = pixels.foldLeft(ARGB.MaxValue)(_.componentMin(_))
        val max = pixels.foldLeft(ARGB.MinValue)(_.componentMax(_))

        pixels.map(p => scaleAtoB(p, min, max))
    }

    def verticalSobel(pix: (Int, Int) => ARGB, x: Int, y: Int): Double = {
        pix(x - 1, y - 1).r *  1 + pix(x, y - 1).r *  2 + pix(x + 1, y - 1).r *  1 +
        pix(x - 1, y + 1).r * -1 + pix(x, y - 1).r * -2 + pix(x + 1, y + 1).r * -1
    }

    def horizontalSobel(pix: (Int, Int) => ARGB, x: Int, y: Int): Double = {
        pix(x - 1, y - 1).r * -1 + pix(x + 1, y - 1).r * 1 +
        pix(x - 1, y    ).r * -2 + pix(x + 1, y    ).r * 2 +
        pix(x - 1, y + 1).r * -1 + pix(x + 1, y + 1).r * 1
    }

    def sobelValues(width: Int, height: Int, colorPixels: Array[ARGB], cont: Continuation): Array[(Double, Double)] = {
        val grayPixels = colorPixels.map(grayscale)
        val pix = pixel(grayPixels, cont, width, height) _

        (for {
            y <- 0 until height
            x <- 0 until width
        } yield {
            val gx = horizontalSobel(pix, x, y)
            val gy = verticalSobel(pix, x, y)
            val g = math.sqrt(gx * gx + gy * gy)
            val angle = math.atan2(gy, gx) + math.Pi
            (g, angle)
        }).toArray
    }

    def sobel(width: Int, height: Int, pixels: Array[ARGB], cont: Continuation): Array[ARGB] = {

        val values = sobelValues(width, height, pixels, cont)
        val newPixels = for ((g, angle) <- values) yield {
            extractChannels(HSBtoRGB(math.toDegrees(angle).toFloat, 1, g.toFloat))
        }
        normalizeScale(newPixels)
    }

    def inBounds(x: Int, y: Int, width: Int, height: Int): Boolean = x >= 0 && x < width && y >= 0 && y < height

    def hysteresisSobel(low: Double, high: Double)(width: Int, height: Int, pixels: Array[ARGB], cont: Continuation): Array[ARGB] = {
        val values = sobelValues(width, height, pixels, cont)
        val neighbors = Array(
            ((-1, 0), (1, 0)),
            ((-1, 1), (1, -1)),
            ((0, -1), (0, 1)),
            ((-1, -1), (1, 1))
        )
        val HighState = 2.toByte
        val LowState = 1.toByte
        val OffState = 0.toByte

        def valAt(x: Int, y: Int): Double = {
            if (inBounds(x, y, width, height)) values(y * width + x)._1
            else Double.MinValue
        }

        @tailrec
        def hysteresisThresholding(states: Array[Byte]): Array[Byte] = {

            def isStrong(x: Int, y: Int): Boolean =
                if (inBounds(x, y, width, height)) states(y * width + x) == HighState
                else false


            def hasHighNeighbor(x: Int, y: Int): Boolean =
                neighbors.exists {
                    case ((ax, ay), (bx, by)) => isStrong(x + ax, y + ay) || isStrong(x + bx, y + by)
                }

            val newStates = (for {
                y <- 0 until height
                x <- 0 until width
            } yield {
                states(y * width + x) match {
                    case HighState => HighState
                    case LowState if hasHighNeighbor(x, y) => HighState
                    case LowState => LowState
                    case OffState => OffState
                }
            }).toArray

            // newStates and states are not sorted, but order retaining, so sameElements is safe to use
            if (newStates sameElements states) states
            else hysteresisThresholding(newStates)
        }

        val localMaxima = (for {
            y <- 0 until height
            x <- 0 until width
        } yield {
            val (g, angle) = values(y * width + x)
            val ((ax, ay), (bx, by)) = neighbors((((math.toDegrees(angle).toInt + 360) % 360) / (360 / 8)) % neighbors.length)

            if (valAt(x + ax, y + ay) > g || valAt(x + bx, y + by) > g) 0
            else g
        }).toArray

        val min = localMaxima.min(Ordering.Double.TotalOrdering)
        val max = localMaxima.max(Ordering.Double.TotalOrdering)
        val normalizedLocalMaxima = localMaxima.map(m => scaleAtoB(m, min, max))

        val thresholdStates = normalizedLocalMaxima.map {
            case n if n >= high => HighState
            case n if n >= low && n < high => LowState
            case _ => OffState
        }

        val finalStates = hysteresisThresholding(thresholdStates)

        finalStates.map {
            case HighState => ARGB.one
            case _ => ARGB.zero
        }


    }

    def scaleAtoB[T](valueA: T, minA: T, maxA: T)(implicit frac: Fractional[T]): T = {
        scaleAtoB(valueA, minA, maxA, frac.zero, frac.one)(frac)
    }

    def scaleAtoB[T](valueA: T, minA: T, maxA: T, minB: T, maxB: T)(implicit frac: Fractional[T]): T = {
        val rangeA = maxA - minA
        val rangeB = maxB - minB
        val valueB = (frac.div(valueA - minA, rangeA) * rangeB) + minB
        valueB
    }

}
