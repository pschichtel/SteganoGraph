package tel.schich

import java.awt.image.BufferedImage
import java.io.OutputStream
import java.nio.file.Files.{newInputStream, newOutputStream}
import java.nio.file.Paths
import java.nio.file.StandardOpenOption.{CREATE, READ, TRUNCATE_EXISTING, WRITE}

import javax.imageio.ImageIO

package object steganograph {

    case class ARGB(a: Double, r: Double, g: Double, b: Double) {
        def * (w: Double): ARGB = ARGB(a * w, r * w, g * w, b * w)

        def componentMin(other: ARGB) = ARGB(math.min(a, other.a), math.min(r, other.r), math.min(g, other.g), math.min(b, other.b))
        def componentMax(other: ARGB) = ARGB(math.max(a, other.a), math.max(r, other.r), math.max(g, other.g), math.max(b, other.b))

        def magnitudeSquared: Double = a * a + r * r + g * g + b * b
        def distanceSquared(other: ARGB): Double = math.pow(a - other.a, 2) + math.pow(r - other.r, 2) + math.pow(g - other.g, 2) + math.pow(b - other.b, 2)
    }
    implicit object ARGB extends Integral[ARGB] {
        val A: ARGB = ARGB(1, 0, 0, 0)
        val R: ARGB = ARGB(0, 1, 0, 0)
        val G: ARGB = ARGB(0, 0, 1, 0)
        val B: ARGB = ARGB(0, 0, 0, 1)

        override def plus(x: ARGB, y: ARGB): ARGB = ARGB(x.a + y.a, x.r + y.r, x.g + y.g, x.b + y.b)

        override def minus(x: ARGB, y: ARGB): ARGB = ARGB(x.a - y.a, x.r - y.r, x.g - y.g, x.b - y.b)

        override def times(x: ARGB, y: ARGB): ARGB = ARGB(x.a * y.a, x.r * y.r, x.g * y.g, x.b * y.b)

        override def quot(x: ARGB, y: ARGB): ARGB = ARGB(x.a / y.a, x.r / y.r, x.g / y.g, x.b / y.b)

        override def rem(x: ARGB, y: ARGB): ARGB = ARGB(x.a % y.a, x.r % y.r, x.g % y.g, x.b % y.b)

        override def negate(x: ARGB): ARGB = ARGB(-x.a, -x.r, -x.g, -x.b)

        override def fromInt(x: Int): ARGB = ARGB(1, x, x, x)

        override def toInt(x: ARGB): Int = composeChannels(x.a, x.r, x.g, x.b)

        override def toLong(x: ARGB): Long = toInt(x)

        override def toFloat(x: ARGB): Float = toInt(x)

        override def toDouble(x: ARGB): Double = toInt(x)

        override def compare(x: ARGB, y: ARGB): Int = zero.distanceSquared(x).compareTo(zero.distanceSquared(y))

        override def abs(x: ARGB): ARGB = ARGB(math.abs(x.a), math.abs(x.r), math.abs(x.g), math.abs(x.b))

        override val zero: ARGB = ARGB(0, 0, 0, 0)

        override val one: ARGB = ARGB(1, 1, 1, 1)

        val MaxValue: ARGB = ARGB(Double.MaxValue, Double.MaxValue, Double.MaxValue, Double.MaxValue)
        val MinValue: ARGB = ARGB(Double.MinValue, Double.MinValue, Double.MinValue, Double.MinValue)
    }

    val OutputFormat = "PNG"

    private val bitsPerPixel = 8
    private val secretBitCount = 1
    private val publicBitCount = bitsPerPixel - secretBitCount

    private val publicBits = (0xFF << secretBitCount) & 0xFF

    def crossover(pub: Double, sec: Double): Double =
        crossover((pub * 255.0).round.toInt, (sec * 255.0).round.toInt) / 255.0

    def crossover(pub: Int, sec: Int): Int =
        (pub & publicBits) | (sec >>> publicBitCount)

    def split(pixChan: Double): (Double, Double) = {
        val (l, r) = split((pixChan * 255).round.toInt)
        (l / 255.0, r / 255.0)
    }

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
        ARGB(((argb >>> 24) & 0xFF) / 255.0, ((argb >>> 16) & 0xFF) / 255.0, ((argb >>> 8) & 0xFF) / 255.0, (argb & 0xFF) / 255.0)

    def composeChannels(a: Double, r: Double, g: Double, b: Double): Int = {
        composeChannels((a * 255.0).round.toInt, (r * 255.0).round.toInt, (g * 255.0).round.toInt, (b * 255.0).round.toInt)
    }

    def composeChannels(a: Int, r: Int, g: Int, b: Int): Int =
        ((a & 0xFF) << 24) | ((r & 0xFF) << 16) | ((g & 0xFF) << 8) | (b & 0xFF)
}
