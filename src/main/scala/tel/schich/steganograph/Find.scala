package tel.schich.steganograph

import java.awt.image.BufferedImage

object Find {

    def main(args: Array[String]): Unit = {

        val input = readImg(args(0))
        val output = new BufferedImage(input.getWidth, input.getHeight, input.getType)

        for {
            x <- 0 until input.getWidth()
            y <- 0 until input.getHeight()
        } {
            val pixel = input.getRGB(x, y)
            val ARGB(a, r, g, b) = extractChannels(pixel)

            val (_, secR) = split(r)
            val (_, secG) = split(g)
            val (_, secB) = split(b)
            output.setRGB(x, y, composeChannels(a, secR, secG, secB))
        }

        writeImg(output, args(1))

    }
}
