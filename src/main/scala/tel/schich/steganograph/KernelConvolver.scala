package tel.schich.steganograph

import java.awt.image.BufferedImage

object KernelConvolver {

    def main(args: Array[String]): Unit = {
        val kernels = args.drop(2).map(_.trim.toLowerCase).map {
            case "dither" => dither(closest(Seq(ARGB.zero, ARGB.A, ARGB.R, ARGB.G, ARGB.B, ARGB.RG, ARGB.RB, ARGB.GB, ARGB.one))) _
            case "alpha" => channel(ARGB.A) _
            case "red" => channel(ARGB.R) _
            case "green" => channel(ARGB.G) _
            case "blue" => channel(ARGB.B) _
            case "red_green" => channel(ARGB.RG) _
            case "red_blue" => channel(ARGB.RB) _
            case "green_blue" => channel(ARGB.GB) _
            case "grayscale" => grayscale _
            case "identity" => kernel(
                0, 0, 0,
                0, 1, 0,
                0, 0, 0
            ) _
            case "mean3" => kernel(
                1, 1, 1,
                1, 1, 1,
                1, 1, 1
            ) _
            case "mean" => kernel(
                1, 1, 1, 1, 1,
                1, 1, 1, 1, 1,
                1, 1, 1, 1, 1,
                1, 1, 1, 1, 1,
                1, 1, 1, 1, 1
            ) _
            case "emboss" => kernel(
                -2, -1, 0,
                -1,  1, 1,
                 0,  1, 2
            ) _
            case "gauss3" => kernel(
                1, 2, 1,
                2, 4, 2,
                1, 2, 1
            ) _
            case "gauss" => kernel(
                1,  4,  6,  4, 1,
                1, 16, 24, 16, 4,
                1, 24, 36, 24, 6,
                1, 16, 24, 16, 4,
                1,  4,  6,  4, 1
            ) _
            case "edge" => kernel(
                -1, -1, -1,
                -1,  8, -1,
                -1, -1, -1
            ) _
            case "edge_x" => kernel(
                -1, 0, 1,
                -1, 0, 1,
                -1, 0, 1
            ) _
            case "edge_y" => kernel(
                -1, -1, -1,
                 0,  0, 0,
                 1,  1,  1
            ) _
            case "sharpen" => kernel(
                 0, -1,  0,
                -1,  5, -1,
                 0, -1,  0
            ) _
        }

        val img = readImg(args(0))
        val width = img.getWidth
        val height = img.getHeight
        val pixels = (for {
            y <- 0 until height
            x <- 0 until width
        } yield extractChannels(img.getRGB(x, y))).toArray

        val processed = kernels.foldLeft(pixels)((pixels, filter) => filter(width, height, pixels))

        val output = new BufferedImage(width, height, img.getType)
        for {
            imgY <- 0 until height
            imgX <- 0 until width
        } {
            output.setRGB(imgX, imgY, processed(imgY * width + imgX).toARGB32)
        }

        writeImg(output, args(1))
    }

    def dither(mapColor: ARGB => ARGB)(width: Int, height: Int, pixels: Array[ARGB]): Array[ARGB] = {
        val out = Array.ofDim[ARGB](pixels.length)
        Array.copy(pixels, 0, out, 0, pixels.length)

        def idx(x: Int, y: Int) = y * width + x

        def update(x: Int, y: Int, err: ARGB): Unit = {
            val i = idx(x, y)
            if (out.isDefinedAt(i)) {
                out(i) = out(i) add err
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
            update(x + 1, y    , err * 7.0/16.0)
            update(x - 1, y + 1, err * 3.0/16.0)
            update(x    , y + 1, err * 5.0/16.0)
            update(x + 1, y + 1, err * 1.0/16.0)
        }

        out
    }

    def closest(palette: Seq[ARGB])(search: ARGB): ARGB =
        palette.minBy(_.distanceSquared(search))

    def channel(c: ARGB)(width: Int, height: Int, pixels: Array[ARGB]): Array[ARGB] =
        pixels.map(_ * c)

    def grayscale(width: Int, height: Int, pixels: Array[ARGB]): Array[ARGB] =
        pixels.map(_.toGray)

    def kernel(kernel: Double*)(width: Int, height: Int, pixels: Array[ARGB]): Array[ARGB] =
        applyKernel(width, height, pixels, kernel.toArray)

    def applyKernel(width: Int, height: Int, pixels: Array[ARGB], kernel: Array[Double]): Array[ARGB] = {
        val kernelSize = math.sqrt(kernel.length).toInt
        val kernelRadius = kernelSize / 2 // integer division to round down
        val kernelOffsets = (for {
            x <- 0 until kernelSize
            y <- 0 until kernelSize
        } yield {
            (kernel(y * kernelSize + x), x - kernelRadius, y - kernelRadius)
        }).toArray

        val processed = (for {
            imgY <- 0 until height
            imgX <- 0 until width
        } yield {
            (for ((w, x, y) <- kernelOffsets) yield {
                val actualX = imgX + x
                val actualY = imgY + y

                if (actualX >= 0 && actualX < width && actualY >= 0 && actualY < height) {
                    pixels(actualY * width + actualX) * w
                } else ARGB.zero
            }).sum
        }).toArray

        val min = processed.foldLeft(ARGB.MaxValue)(_.componentMin(_))
        val max = processed.foldLeft(ARGB.MinValue)(_.componentMax(_))
        val diff = max - min

        processed.map(_.lerp(diff))
    }

}
