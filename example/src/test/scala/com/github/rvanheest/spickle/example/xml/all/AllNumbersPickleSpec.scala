package com.github.rvanheest.spickle.example.xml.all

import java.nio.file.Paths

import com.github.rvanheest.spickle.example.xml.all.AllNumberPickle._
import com.github.rvanheest.spickle.pickle.xml.XmlPickle._
import org.scalatest.{ FlatSpec, Inside, Matchers }

import scala.util.Success
import scala.xml._

class AllNumbersPickleSpec extends FlatSpec with Matchers with Inside {

  "pickleNumbers" should "pickle when the optional values are all present" in {
    val path = Paths.get(getClass.getResource("/all/numbers1.xml").toURI)
    val xmlBefore = Utility.trim(XML.loadFile(path.toFile))

    inside(pickleNumbers.parse(xmlBefore)) {
      case (Success(result), remainder) =>
        remainder shouldBe empty

        inside(pickleNumbers.serialize(result)) {
          case Success(Seq(xmlAfter)) =>
            val xmlExpected = <numbers>
              <a>1</a>
              <b>2</b>
              <c>3</c>
              <d>4</d>
              <e>5</e>
              <f>6</f>
              <g>7</g>
              <h>8</h>
              <i>9</i>
              <j>10</j>
              <k>11</k>
              <l>12</l>
              <m>13</m>
              <n>14</n
              ><o>15</o>
              <p>16</p>
              <q>17</q>
              <r>18</r>
              <s>19</s>
              <t>20</t>
              <u>21</u>
              <v>22</v>
              <w>23</w>
              <x>24</x>
              <y>25</y>
              <z>26</z>
            </numbers>

            Utility.trim(xmlAfter) shouldBe Utility.trim(xmlExpected)
        }
    }
  }

  it should "pickle when the optional values are not all present" in {
    val path = Paths.get(getClass.getResource("/all/numbers2.xml").toURI)
    val xmlBefore = Utility.trim(XML.loadFile(path.toFile))

    inside(pickleNumbers.parse(xmlBefore)) {
      case (Success(result), remainder) =>
        remainder shouldBe empty

        inside(pickleNumbers.serialize(result)) {
          case Success(Seq(xmlAfter)) =>
            val xmlExpected = <numbers>
              <b>2</b>
              <d>4</d>
              <e>5</e>
              <f>6</f>
              <g>7</g>
              <h>8</h>
              <i>9</i>
              <j>10</j>
              <k>11</k>
              <l>12</l>
              <m>13</m>
              <n>14</n
              ><o>15</o>
              <p>16</p>
              <q>17</q>
              <r>18</r>
              <s>19</s>
              <t>20</t>
              <u>21</u>
              <v>22</v>
              <w>23</w>
              <x>24</x>
              <y>25</y>
              <z>26</z>
            </numbers>

            Utility.trim(xmlAfter) shouldBe Utility.trim(xmlExpected)
        }
    }
  }
}
