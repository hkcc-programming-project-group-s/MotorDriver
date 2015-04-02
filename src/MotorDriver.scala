/**
 * Created by beenotung on 4/3/15.
 */
class MotorDriver {
  /**
   *
   * @param direction
   * range from -1.0 to 1.0
   * @return
   * left motor pwm (range from -1.0 to 1.0)
   * right motor pwm (range from -1.0 to 1.0)
   */
  def getCommand(direction: Double): (Double, Double) = {
    null
  }

  /**
   *
   * @param left
   * left motor pwm (range from -1.0 to 1.0)
   * @param right
   * right motor pwm (range from -1.0 to 1.0)
   * @return
   * direction (range from -1.0 to 1.0)
   */
  def getDirection(left: Double, right: Double): Double = {
    null
  }
}
