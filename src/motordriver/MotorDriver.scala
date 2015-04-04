package motordriver

import motordriver.MotorDriver._
import neuroevolution.NeuroEvolution
import neuroevolution.geneticalgorithm.ProblemType.Minimize
import neuroevolution.neuralnetwork.{Perceptron, TweakedCosine}

import scala.collection.mutable.ArrayBuffer

/**
 * Created by beenotung on 4/3/15.
 */

object MotorDriver {

  /**
   *
   * @param double
   * -1..1
   * @return
   * 0..1
   */
  def reScale(double: Double) = {
    (double + 1) / 2d
  }

  /**
   *
   * @param double
   * 0..1
   * @return
   * -1..1
   */
  def reversedScale(double: Double) = {
    (double - 0.5) * 2
  }
}

class MotorDriver extends Thread {


  /**
   * input: direction (range from 0 to 1)
   * output: left & right motor pwm (range from 0 to 1)
   */
  val ai = new NeuroEvolution(n_Bit_Weight = 4, n_Bit_Bias = 4, numberOfNodes = Array(1, 8,8, 1), activationFunction = TweakedCosine,
    popSize = 32, pSelection = 0.25, pMutationPow = 2, aMutationPow = 8, parent_immutable = true,
    get_perceptron_inputs = get_perceptron_inputs, eval_perceptron_function = eval_perceptron_function,
    problemType = Minimize,
    diversityWeight = 0.1,
    loopInterval = 0) {
    override def evalFitness_function(rawCode: Array[Boolean]): Double = {
      val perceptron: Perceptron = converter.decode(rawCode)
      val inputs: Array[Array[Double]] = get_perceptron_inputs
      val output_fitness: Array[Double] = Array.fill[Double](inputs.length)(0d)
      inputs.indices.par.foreach(i => output_fitness(i) = checkScore(i, perceptron.run(inputs(i))))
      val avg: Double = output_fitness.sum * 1d / output_fitness.length
      var overallFitness: Double = avg
      output_fitness.foreach(fitness => overallFitness += Math.pow(fitness - avg, 2))
      overallFitness + diffCheck(perceptron) * 1000d
    }
  }

  def diffCheck(perceptron: Perceptron): Double = {
    val input = Utils.random.nextDouble()
    val output1 = perceptron.run(Array(input))
    val output2 = perceptron.run(Array(input + 11.25d / 360d))
    Math.pow(output1(0) - output2(0),2)// + Math.pow(output1(1) - output2(1),2)
  }

  def checkScore(i: Int, outputs: Array[Double]): Double = {
    Math.pow(reScale(sampleCommandPairs(i).left) - outputs(0), 2) //+ Math.pow(reScale(sampleCommandPairs(i).right) - outputs(1), 2)
  }

  //(direction,left,right)
  var sampleCommandPairs = new ArrayBuffer[CommandPair]

  setup


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
    var nowTime: Long = 0
    var lastTime: Long = nowTime
    var nowRound: Int = -200
    var lastRound: Int = nowRound
    var fileCount = 1
    var roundPerSecond = 0d
    val alpha = 0.5
    var nowBest = ai.ga.getBestGene
    var lastBest = nowBest
    var estimatedRoundLeft = 100d
    while (true) {
      lastTime = nowTime
      nowTime = System.currentTimeMillis()
      lastRound = nowRound
      nowRound = ai.ga.round
      lastBest = nowBest
      nowBest = ai.ga.getBestGene
      roundPerSecond = roundPerSecond * (1 - alpha) + alpha * (nowRound - lastRound) * 1000d / (nowTime - lastTime)
      var message = "\nRound: \t" + nowRound
      message += "\n" + Math.round(roundPerSecond) + " rounds per Second"
      message += "\nBest's fitness: \t" + Math.sqrt(nowBest.getFitness / 2d / sampleCommandPairs.length)
      message += "\nOverall diversity: \t" + (1 - ai.ga.overallDiversity)
      message += "\nImprovement: \t\t" + Math.round((nowBest.fitness - lastBest.fitness) / lastBest.fitness * 100) / 100d + "%"
      estimatedRoundLeft = estimatedRoundLeft * (1 - alpha) + alpha * Math.abs(nowBest.fitness / ((lastBest.fitness - nowBest.fitness + 0.00001) / (lastTime - nowTime)) * roundPerSecond)
      message += "\nEstimate " + estimatedRoundLeft + " round left"
      println(message)
      //log.writeObject(best.rawCode)
      fileCount = (fileCount + 1) % 2
      /*printToFile(new File(fileCount + "-MotorDriver.perceptron")) { p =>
        nowBest.rawCode.foreach(b =>
          if (b) p.print(1)
          else p.print(0)
        )
        p.println()
      }*/
      ai.saveBestToFile(fileCount + "-MotorDriver", false)
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

  class CommandPair(val direction: Double, val left: Double, val right: Double)

}

