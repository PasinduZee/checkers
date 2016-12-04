package checkers.core

import checkers.logger

class ScreenLayoutAdapter extends ScreenLayoutSettingsProvider {

  private case class CacheSettings(width: Int,
                                   height: Int,
                                   settings: ScreenLayoutSettings)

  private var cache: CacheSettings = _

  private val thresholds = Vector(
    (1024, 896),
    (924, 796),
    (824, 696),
    (724, 596))

  private lazy val levels = Vector(
    // large
    new DefaultScreenLayoutSettings { },

    new DefaultScreenLayoutSettings {
      override def GameSceneWidthPixels: Int = 700

      override def GameSceneHeightPixels: Int = 700

      override def TopChromeHeightPixels: Int = 80

      override def SideChromeButtonHeightPixels: Int = 42
    },

    new DefaultScreenLayoutSettings {
      override def GameSceneWidthPixels: Int = 600

      override def GameSceneHeightPixels: Int = 600

      override def TopChromeHeightPixels: Int = 70

      override def SideChromeButtonHeightPixels: Int = 36
    },

    new DefaultScreenLayoutSettings {
      override def GameSceneWidthPixels: Int = 500

      override def GameSceneHeightPixels: Int = 500

      override def TopChromeHeightPixels: Int = 60

      override def SideChromeButtonHeightPixels: Int = 30
    }
  ).lift

  private lazy val smallest = new DefaultScreenLayoutSettings {
    override def GameSceneWidthPixels: Int = 400

    override def GameSceneHeightPixels: Int = 400

    override def TopChromeHeightPixels: Int = 50

    override def SideChromeButtonHeightPixels: Int = 24
  }

  private def getSettings(width: Int, height: Int): ScreenLayoutSettings = {
    val idx = thresholds.indexWhere { case (minWidth, minHeight) =>
      width >= minWidth && height >= minHeight
    }
    levels(idx).getOrElse(smallest)
  }

  override def getScreenLayoutSettings(viewPortInfo: ViewPortInfo): ScreenLayoutSettings = {
    val width = viewPortInfo.viewPortWidth.toInt
    val height = viewPortInfo.viewPortHeight.toInt

    if(cache != null && cache.width == width && cache.height == height) cache.settings
    else {
      val newSettings = getSettings(width, height)

      logger.log.info(s"Adapting screen layout for dimensions ($width × $height)")

      cache = CacheSettings(width, height, newSettings)
      newSettings
    }
  }
}