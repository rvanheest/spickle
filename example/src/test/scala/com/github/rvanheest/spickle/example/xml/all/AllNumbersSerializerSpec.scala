package com.github.rvanheest.spickle.example.xml.all

import com.github.rvanheest.spickle.example.xml.all.All.Numbers
import com.github.rvanheest.spickle.example.xml.all.AllNumberSerializer._
import com.github.rvanheest.spickle.serializer.xml.XmlSerializer._
import org.scalatest.{ FlatSpec, Inside, Matchers }

import scala.util.Success
import scala.xml.Utility

class AllNumbersSerializerSpec extends FlatSpec with Matchers with Inside {

  "serializeNumbers" should "serialize when the optional values are all present" in {
    val numbers = Numbers(Option(1), Option(2), Option(3), Option(4), 5, 6, 7, 8, 9, 10,
      11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26)

    inside(serializeNumbers.serialize(numbers)) {
      case Success(xml) =>
        xml should contain only Utility.trim(
          <numbers>
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
            <n>14</n>
            <o>15</o>
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
        )
    }
  }

  it should "serialize when the optional values are not all present" in {
    val numbers = Numbers(None, Option(2), None, Option(4), 5, 6, 7, 8, 9, 10,
      11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
      21, 22, 23, 24, 25, 26)

    inside(serializeNumbers.serialize(numbers)) {
      case Success(xml) =>
        xml should contain only Utility.trim(
          <numbers>
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
            <n>14</n>
            <o>15</o>
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
        )
    }
  }
}
