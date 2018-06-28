package tel.schich

import java.awt.image.BufferedImage
import java.io.OutputStream
import java.nio.file.Files.{newInputStream, newOutputStream}
import java.nio.file.Paths
import java.nio.file.StandardOpenOption.{CREATE, READ, TRUNCATE_EXISTING, WRITE}

import javax.imageio.ImageIO

package object steganograph {

    case class ARGB(a: Int, r: Int, g: Int, b: Int) {
        def scale(w: Double) = ARGB((a * w).toInt, (r * w).toInt, (g * w).toInt, (b * w).toInt)
        @inline
        def * (w: Double) = scale(w)
        @inline
        def / (w: Double) = scale(1/w)

        def toARGB32 = composeChannels(a, r, g, b)

        def toGray = {
            val avg = (r + g + b) / 3
            ARGB(1, avg, avg, avg)
        }

        def componentMin(other: ARGB) = ARGB(math.min(a, other.a), math.min(r, other.r), math.min(g, other.g), math.min(b, other.b))

        def componentMax(other: ARGB) = ARGB(math.max(a, other.a), math.max(r, other.r), math.max(g, other.g), math.max(b, other.b))

        def lerp(min: ARGB, max: ARGB, range: Int): ARGB =
            lerp(max - min, range)

        def lerp(diff: ARGB, range: Int): ARGB = ARGB(
            ((a.toDouble / diff.a.toDouble) * range.toDouble).toInt,
            ((r.toDouble / diff.r.toDouble) * range.toDouble).toInt,
            ((g.toDouble / diff.g.toDouble) * range.toDouble).toInt,
            ((b.toDouble / diff.b.toDouble) * range.toDouble).toInt
        )
    }
    implicit object ARGB extends Integral[ARGB] {
        val A = ARGB(1, 0, 0, 0)
        val R = ARGB(0, 1, 0, 0)
        val RG = ARGB(0, 1, 1, 0)
        val RB = ARGB(0, 1, 0, 1)
        val G = ARGB(0, 0, 1, 0)
        val GB = ARGB(0, 0, 1, 1)
        val B = ARGB(0, 0, 0, 1)

        override def plus(x: ARGB, y: ARGB): ARGB = ARGB(x.a + y.a, x.r + y.r, x.g + y.g, x.b + y.b)

        override def minus(x: ARGB, y: ARGB): ARGB = ARGB(x.a - y.a, x.r - y.r, x.g - y.g, x.b - y.b)

        override def times(x: ARGB, y: ARGB): ARGB = ARGB(x.a * y.a, x.r * y.r, x.g * y.g, x.b * y.b)

        override def quot(x: ARGB, y: ARGB): ARGB = ARGB(x.a / y.a, x.r / y.r, x.g / y.g, x.b / y.b)

        override def rem(x: ARGB, y: ARGB): ARGB = ARGB(x.a % y.a, x.r % y.r, x.g % y.g, x.b % y.b)

        override def negate(x: ARGB): ARGB = ARGB(-x.a, -x.r, -x.g, -x.b)

        override def fromInt(x: Int): ARGB = ARGB(1, x, x, x)

        override def toInt(x: ARGB): Int = x.toARGB32

        override def toLong(x: ARGB): Long = x.toARGB32

        override def toFloat(x: ARGB): Float = x.toARGB32

        override def toDouble(x: ARGB): Double = x.toARGB32

        override def compare(x: ARGB, y: ARGB): Int = x.toARGB32 - y.toARGB32

        override val zero: ARGB = ARGB(0, 0, 0, 0)

        override val one: ARGB = ARGB(1, 1, 1, 1)
    }

    val OutputFormat = "PNG"

    private val bitsPerPixel = 8
    private val secretBitCount = 1
    private val publicBitCount = bitsPerPixel - secretBitCount

    private val publicBits = (0xFF << secretBitCount) & 0xFF

    def crossover(pub: Int, sec: Int): Int =
        (pub & publicBits) | (sec >>> publicBitCount)

    def split(pixChan: Int): (Int, Int) =
        (pixChan & publicBits, (pixChan << publicBitCount) & 0xFF)

    def loan[A <: AutoCloseable, B](a: A)(f: A => B): B =
        try f(a)
        finally a.close()

    def readImg(path: String): BufferedImage =
        loan(newInputStream(Paths.get(path), READ))(ImageIO.read)

    def writeImg(img: BufferedImage, path: String, format: String = OutputFormat): Boolean =
        loan(newOutputStream(Paths.get(path), TRUNCATE_EXISTING, CREATE, WRITE))(ImageIO.write(img, format, _: OutputStream))


    def extractChannels(argb: Int): ARGB =
        ARGB((argb >>> 24) & 0xFF, (argb >>> 16) & 0xFF, (argb >>> 8) & 0xFF, argb & 0xFF)

    def composeChannels(a: Int, r: Int, g: Int, b: Int): Int =
        ((a & 0xFF) << 24) | ((r & 0xFF) << 16) | ((g & 0xFF) << 8) | (b & 0xFF)
}
