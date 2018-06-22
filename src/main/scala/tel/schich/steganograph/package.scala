package tel.schich

import java.awt.image.BufferedImage
import java.io.OutputStream
import java.nio.file.Files.{newInputStream, newOutputStream}
import java.nio.file.Paths
import java.nio.file.StandardOpenOption.{CREATE, READ, TRUNCATE_EXISTING, WRITE}

import javax.imageio.ImageIO

package object steganograph {

    case class ARGB(a: Int, r: Int, g: Int, b: Int)

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
        try f()
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
