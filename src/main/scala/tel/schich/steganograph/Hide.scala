package tel.schich.steganograph

import java.awt.image.BufferedImage

object Hide {

    def main(args: Array[String]): Unit = {

        val publicImage = readImg(args(0))
        val secretImage = readImg(args(1))
        val output = new BufferedImage(publicImage.getWidth, publicImage.getHeight, publicImage.getType)

        for {
            x <- 0 until publicImage.getWidth()
            y <- 0 until publicImage.getHeight()
        } {
            val publicPixel = publicImage.getRGB(x, y)
            val ARGB(a, publicR, publicG, publicB) = extractChannels(publicPixel)
            val secretPixel = secretImage.getRGB(x % secretImage.getTileWidth, y % secretImage.getHeight)
            val ARGB(_, secretR, secretG, secretB) = extractChannels(secretPixel)

            val r = crossover(publicR, secretR)
            val g = crossover(publicG, secretG)
            val b = crossover(publicB, secretB)
            output.setRGB(x, y, composeChannels(a, r, g, b))
        }

        writeImg(output, args(2))
    }
}
