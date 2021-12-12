import scala.io.Source
import scala.collection._
import java.util.ArrayList
import scala.collection.mutable.ListMap
import scala.collection.mutable.ListBuffer
import java.io._


object ex_3 {

  def main(args: Array[String]){
    var shin = new JaccardSim()
    shin.Tests()
  }

  class JaccardSim(){

    def Tests(){
      var res: Float = 0

      var pw = new FileWriter("JaccardResult.txt") // file to save results
      pw.write("Book 1 \t Book 2 \t Jaccard Similarity\n")

      var b1 = "Bible_1.txt"
      var b2 = "Bible_2.txt"
      var b3 = "Bible_3.txt"
      var b4 = "Alice.txt"
      var b5 = "Frankenstein.txt"
      var b6 = "A-Christmas-Carol.txt"

      var t1 = TextReader(b1)
      var t2 = TextReader(b2)
      var t3 = TextReader(b3)
      var t4 = TextReader(b4)
      var t5 = TextReader(b5)
      var t6 = TextReader(b6)

      var sh1: Set[String] = Set()
      var sh2: Set[String] = Set()
      var sh3: Set[String] = Set()
      var sh4: Set[String] = Set()
      var sh5: Set[String] = Set()
      var sh6: Set[String] = Set()

      for(i<-4 to 13){ //calculate jaccard similarity between each pair of documents for one shingle size
        pw.write("Shingle size: " + i + "\n")

        sh1 = Shingles(t1, i)
        sh2 = Shingles(t2, i)
        sh3 = Shingles(t3, i)
        sh4 = Shingles(t4, i)
        sh5 = Shingles(t5, i)
        sh6 = Shingles(t6, i)

        res = JaccardSimilarity(sh1, sh2)
        pw.write(b1 + "\t" + b2 + "\t" + res + "\n")
        res = JaccardSimilarity(sh1, sh3)
        pw.write(b1 + "\t" + b3 + "\t" + res + "\n")
        res = JaccardSimilarity(sh1, sh4)
        pw.write(b1 + "\t" + b4 + "\t" + res + "\n")
        res = JaccardSimilarity(sh1, sh5)
        pw.write(b1 + "\t" + b5 + "\t" + res + "\n")
        res = JaccardSimilarity(sh1, sh6)
        pw.write(b1 + "\t" + b6 + "\t" + res + "\n")
        res = JaccardSimilarity(sh2, sh3)
        pw.write(b2 + "\t" + b3 + "\t" + res + "\n")
        res = JaccardSimilarity(sh2, sh4)
        pw.write(b2 + "\t" + b4 + "\t" + res + "\n")
        res = JaccardSimilarity(sh2, sh5)
        pw.write(b2 + "\t" + b5 + "\t" + res + "\n")
        res = JaccardSimilarity(sh2, sh6)
        pw.write(b2 + "\t" + b6 + "\t" + res + "\n")
        res = JaccardSimilarity(sh3, sh4)
        pw.write(b3 + "\t" + b4 + "\t" + res + "\n")
        res = JaccardSimilarity(sh3, sh5)
        pw.write(b3 + "\t" + b5 + "\t" + res + "\n")
        res = JaccardSimilarity(sh3, sh6)
        pw.write(b3 + "\t" + b6 + "\t" + res + "\n")
        res = JaccardSimilarity(sh4, sh5)
        pw.write(b4 + "\t" + b5 + "\t" + res + "\n")
        res = JaccardSimilarity(sh4, sh6)
        pw.write(b4 + "\t" + b6 + "\t" + res + "\n")
        res = JaccardSimilarity(sh5, sh6)
        pw.write(b5 + "\t" + b6 + "\t" + res + "\n")        
      }
      pw.close()
    }

    def JaccardSimilarity(shingle1: Set[String], shingle2: Set[String]): Float = {
      var union = shingle1.union(shingle2) //union of 2 sets
      var intersection = shingle1.intersect(shingle2) //intersection of 2 sets
      var JaccSim: Float = intersection.size.toFloat / union.size.toFloat

      println("Union size: " + union.size + "; Intersection size: " + intersection.size + "; Jaccard sim: " + JaccSim)

      return JaccSim
    }

    def Shingles(text: List[String], len: Int): Set[String] = {
      var shingles_array: mutable.ArrayBuffer[String] = mutable.ArrayBuffer() //empty array for shingles
      var shingle_string: String = "" //string to make a shingle

      for(i<-0 to (text.length - len)){ //for every word in the text
        for(j<-0 to len - 1){ //make one shingle
          shingle_string = shingle_string + text(i+j) + " "
        }
        shingles_array.append(shingle_string) //append the shingle list with one shingle
        shingle_string = "" //clear the help string so we can reuse it
      }

      return shingles_array.toSet //turning array into set to get rid of duplicates
    }

    def TextReader(file_name: String): List[String] = {
      var newLines: mutable.ArrayBuffer[String] = mutable.ArrayBuffer() //empty list that works like in python

      var lines = Source.fromFile(file_name)("UTF-8").getLines.toList //getting text from file

      for(i<-lines){
        newLines.append(i.replaceAll("""[\p{Punct}]"""," ").replaceAll("’"," ").replaceAll("”"," ").replaceAll(" s ", " ")) //removing punctuation, the weird ’ and single "s"
      }
      
      var result = newLines.flatMap(x=>x.split("\\s+").map(x=>x.trim)).map(x=>x.toLowerCase).toList //removing uppercase and whitespaces
      return result
    }
  }
}