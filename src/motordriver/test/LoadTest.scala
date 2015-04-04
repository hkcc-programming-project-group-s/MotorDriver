package motordriver.test

import java.io.FileInputStream

import neuroevolution.NeuroEvolution
import neuroevolution.geneticalgorithm.ProblemType._
import neuroevolution.neuralnetwork.{Perceptron, TweakedCosine}

import scala.collection.mutable.ArrayBuffer
import motordriver.MotorDriver._

/**
 * Created by beenotung on 4/4/15.
 */
object LoadTest extends App {
  override def main(args: Array[String]) {
    val in = new FileInputStream("test.txt")
    val c = new ArrayBuffer[Boolean]
    while (in.available > 0) {
      c += in.read().toChar.equals('1')
    }
    val ai = new NeuroEvolution(n_Bit_Weight = 4, n_Bit_Bias = 4, numberOfNodes = Array(1, 4, 8, 4, 1), activationFunction = TweakedCosine,
      popSize = 8, pSelection = 0.25, pMutationPow = 2, aMutationPow = 4, parent_immutable = true,
      get_perceptron_inputs = null, eval_perceptron_function = null,
      problemType = Minimize,
      diversityWeight = 0.1,
      loopInterval = 0)
    val perceptron = ai.converter.decode(c.toArray)
    val step = 1
    Range(0, 360 + step, step).foreach(degree => test(perceptron, degree))
  }

  def test(perceptron: Perceptron, degree: Double) = {
    val result = perceptron.run(Array(1- degree / 360d))
    println(degree + "\t" + -reversedScale(result(0)) )
  }
}
