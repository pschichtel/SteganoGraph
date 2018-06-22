package tel.schich

import java.awt.image.BufferedImage
import java.nio.file.StandardOpenOption.{CREATE, READ, TRUNCATE_EXISTING, WRITE}
import java.nio.file.{Files, Paths}

import javax.imageio.ImageIO

package object steganograph {

    case class ARGB(a: Int, r: Int, g: Int, b: Int)

    private val bitsPerPixel = 8
    private val secretBitCount = 1
    private val publicBitCount = bitsPerPixel - secretBitCount

    private val publicBits = (0xFF << secretBitCount) & 0xFF

    def crossover(pub: Int, sec: Int): Int =
        (pub & publicBits) | (sec >>> publicBitCount)

    def split(pixChan: Int): (Int, Int) = {
        val pub = pixChan & publicBits
        val sec = (pixChan << publicBitCount) & 0xFF
        (pub, sec)
    }

    def readImg(path: String): BufferedImage = {
        val in = Files.newInputStream(Paths.get(path), READ)
        try {
            ImageIO.read(in)
        } finally {
            in.close()
        }
    }

    def writeImg(img: BufferedImage, path: String): Boolean = {
        val out = Files.newOutputStream(Paths.get(path), TRUNCATE_EXISTING, CREATE, WRITE)
        try {
            ImageIO.write(img, "PNG", out)
        } finally {
            out.close()
        }
    }


    def extractChannels(argb: Int): ARGB = {
        val a = (argb >>> 24) & 0xFF
        val r = (argb >>> 16) & 0xFF
        val g = (argb >>> 8) & 0xFF
        val b = argb & 0xFF
        ARGB(a, r, g, b)
    }

    def composeChannels(a: Int, r: Int, g: Int, b: Int): Int =
        ((a & 0xFF) << 24) | ((r & 0xFF) << 16) | ((g & 0xFF) << 8) | (b & 0xFF)
}
