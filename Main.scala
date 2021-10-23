
import scala.io.Source
import scala.collection._
import java.util.ArrayList
import scala.collection.mutable.ListMap
import scala.collection.mutable.ListBuffer
import java.io._

object Main{

  def main(args: Array[String]){
    //ex_1()
    //ex_2()
    var map1: mutable.Map[String, Int] = mutable.Map()
    val freq1 = new Freq(map1)
    var i = 1
    /*freq1.read_file("Alice.txt")
    freq1.out_console(20)
    println()
    freq1.read_string("Alice")
    freq1.out_console(20)*/

    while(i==1){
      println("What do you want to do? Type:")
      println("\"1\" to input string,")
      println("\"2\" to input a file,")
      println("\"3\" to output frequency of words to console,")
      println("\"4\" to output frequency of words to file,")
      println("\"5\" to exit")
      var input = scala.io.StdIn.readLine()
      
      if(input.toInt==1){
        println("Please input the string")
        input = scala.io.StdIn.readLine()
        freq1.read_string(input)
      }
      else if(input.toInt==2){
        println("Please input the name of the text file - has to be in the same folder as build.sbt")
        input = scala.io.StdIn.readLine()
        freq1.read_file(input)
      }
      else if(input.toInt==3){
        println("How many words do you want to see?")
        input = scala.io.StdIn.readLine()
        freq1.out_console(input.toInt)
      }
      else if(input.toInt==4){
        println("How many words do you want to save to file?")
        input = scala.io.StdIn.readLine()
        println("What should the file be called?")
        var input2 = scala.io.StdIn.readLine()
        freq1.out_file(input.toInt, input2)
      }
      else if(input.toInt==5){
        i=2
      }
      else{
        println("Try again, wrong number")
      }
    }
  }

  //ex3
  class Freq(var words_map: mutable.Map[String, Int]){

    def update_map(new_elements: mutable.ArrayBuffer[String]){
      for(i<-new_elements){ 
        if(!words_map.contains(i)){ //if our map doesnt contain the word we add it to map with value 1
          words_map += (i -> 1)
        }
        else{
          words_map(i) = words_map(i) + 1 // map cointains the word - increase value by 1
        }
      }

      words_map = ListMap(words_map.toSeq.sortWith(_._2 < _._2):_*) //sorting the map by value
    }

    def read_string(string: String){
      var prunned = prunning(string.split(" ").toList) //getting rid of stuff  
      update_map(prunned) 
    }

    def read_file(file_name: String){
      var lines = Source.fromFile(file_name).getLines.toList //getting text from file
      var prunned = prunning(lines)
      update_map(prunned)
    }

    def out_console(item: Int){
      var s = 0

      for(i<-words_map.keys){
        if(s<item){
          println("Word : " + i + "; occurences: " + words_map(i))
          s = s + 1
        }
      }
    }

    def out_file(item: Int, file_name: String){
      var s = 0
      val pw = new FileWriter(new File(file_name + ".csv")) //saving to file

      for(i<-words_map.keys){
        if(s<item){
          pw.write(i + "," + words_map(i) + "\n")
          s = s + 1
        }
      }
      pw.close()
    }

    def prunning(st_array: List[String]): mutable.ArrayBuffer[String] ={
      var newLines: mutable.ArrayBuffer[String] = mutable.ArrayBuffer() //empty list that works like in python

      for(i<-st_array){
        newLines.append(i.replaceAll("""[\p{Punct}]"""," ").replaceAll("’"," ").replaceAll("”"," ").replaceAll(" s ", " ")) //removing punctuation, the weird ’ and single "s"
      }

      var result = newLines.flatMap(x=>x.split("\\s+").map(x=>x.trim)).map(x=>x.toLowerCase).toList //removing uppercase and whitespaces

      var l1: mutable.ArrayBuffer[String] = mutable.ArrayBuffer()

      for(word<- result){ //removing all words of len<3
        if(word.length > 2){
          l1.append(word)
        }
      }

      var stop = Source.fromFile("stop_words_english.txt").getLines.toList.toSet //stopwords
      var l2 = l1.filterNot(stop) //removing stopwords
      return l2 //return prunned list of words
    }
  }

  def ex_2(){
    var lines = Source.fromFile("Alice.txt").getLines.toList //getting text from file
    var newLines: mutable.ArrayBuffer[String] = mutable.ArrayBuffer() //empty list that works like in python

    for(i<-lines){
      newLines.append(i.replaceAll("""[\p{Punct}]"""," ").replaceAll("’"," ").replaceAll("”"," ").replaceAll(" s ", " ")) //removing punctuation, the weird ’ and single "s"
    }

    var result = newLines.flatMap(x=>x.split("\\s+").map(x=>x.trim)).map(x=>x.toLowerCase).toList //removing uppercase

    var l1: mutable.ArrayBuffer[String] = mutable.ArrayBuffer()

    for(word<- result){ //removing all words of len<3
      if(word.length > 2){
        l1.append(word)
      }
    }

    var stop = Source.fromFile("stop_words_english.txt").getLines.toList.toSet //stopwords
    var l2 = l1.filterNot(stop) //removing stopwords
    var l3 = l2.groupBy(identity).mapValues(_.size) //creating a map (word,occurences)    
    var l4 = ListMap(l3.toSeq.sortWith(_._2 > _._2):_*) //sorting the map by number of occurences - highest first

    var s = 0
    val pw = new PrintWriter(new File("Words.txt")) //saving to file

    for(i<-l4.keys){
      if(s<20){
        println("Word : " + i + "; occurences: " + l4(i))
        pw.write(i + "\t")
        s = s + 1
      }
    }

    pw.close
    
  }

  def ex_1(){
    println("Hello, World!")
  }

}