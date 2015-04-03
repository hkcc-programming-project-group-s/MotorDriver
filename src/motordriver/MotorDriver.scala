package motordriver

import java.io._

import neuroevolution.NeuroEvolution
import neuroevolution.geneticalgorithm.ProblemType.Minimize
import neuroevolution.neuralnetwork.TweakedCosine

import scala.collection.mutable.ArrayBuffer


/**
 * Created by beenotung on 4/3/15.
 */
class MotorDriver extends Thread {

  class CommandPair(val direction: Double, val left: Double, val right: Double)

  //(direction,left,right)
  var sampleCommandPairs = new ArrayBuffer[CommandPair]
  setup

  def reScale(double: Double) = {
    (double + 1) / 2d
  }

  def reversedScale(double: Double) = {
    double * 2d - 1
  }


  def setup = {
    sampleCommandPairs += new CommandPair(-1d, -1d, -1d)
    sampleCommandPairs += new CommandPair(-0.75, 0d, -1d)
    sampleCommandPairs += new CommandPair(-0.5, -1d, 1d)
    sampleCommandPairs += new CommandPair(-0.25, 0d, 1d)
    sampleCommandPairs += new CommandPair(0d, 1d, 1d)
    sampleCommandPairs += new CommandPair(0.25, 1d, 0d)
    sampleCommandPairs += new CommandPair(0.5, 1d, -1d)
    sampleCommandPairs += new CommandPair(0.75, -1d, 0d)
    sampleCommandPairs += new CommandPair(1d, -1d, -1d)
  }


  /**
   * input: direction (range from -1 to 1)
   * output: left & right motor pwm (range from -1 to 1)
   */
  val ai = new NeuroEvolution(n_Bit_Weight = 8, n_Bit_Bias = 8, numberOfNodes = Array(1, 8, 16, 8, 2), activationFunction = TweakedCosine,
    popSize = 100, pSelection = 0.25, pMutation = 0.1, aMutation = 0.03, parent_immutable = true,
    get_perceptron_inputs = get_perceptron_inputs, eval_perceptron_function = eval_perceptron_function,
    problemType = Minimize,
    diversityWeight = 0.1,
    loopInterval = 0)

  def get_perceptron_inputs: Array[Array[Double]] = {
    var inputs = ArrayBuffer.empty[Array[Double]]
    sampleCommandPairs.foreach(commandPair => inputs += Array(reScale(commandPair.direction)))
    inputs.toArray
  }

  def eval_perceptron_function(inputs: Array[Double], outputs: Array[Double]): Double = {
    val direction = inputs(0)
    val left = outputs(0)
    val right = outputs(1)
    val target = sampleCommandPairs.filter(sample => reScale(sample.direction) == direction)
    Math.pow(reScale(target(0).left) - left, 2) + Math.pow(reScale(target(0).right) - right, 2)
  }

  override def run = {
    ai.start()
    //val log=new ObjectOutputStream(new FileOutputStream("MotorDriver.perceptron"))
    while (true) {
      val best = ai.ga.getBestGene
      var message = "\nRound: " + ai.ga.round
      message += "\nBest's fitness: " + Math.sqrt(best.getFitness / 2d / sampleCommandPairs.length)
      message += "\nBest's diversity: " + best.diversity
      message += "\nOverall diversity: " + (1 - ai.ga.diversity_weight)
      println(message)
      //log.writeObject(best.rawCode)
      printToFile(new File(ai.ga.round % 2 + "-MotorDriver.perceptron")) { p =>
        best.rawCode.foreach(b =>
          if (b) p.print(1)
          else p.print(0)
        )
        p.println()
      }
      Thread.sleep(1000)
    }
  }

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try {
      op(p)
    } finally {
      p.close()
    }
  }
}

