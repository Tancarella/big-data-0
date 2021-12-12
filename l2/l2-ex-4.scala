import scala.io.Source
import scala.collection._
import java.util.ArrayList
import scala.collection.mutable.ListMap
import scala.collection.mutable.ListBuffer
import java.io._
import scala.util.Random
import scala.util.control.Breaks._

object ex_4 {

    def main(args: Array[String]){
        var t = new EstJacc()
        var r = List.range(4, 13)
        //t.test(10, List("Bible_1.txt", "Bible_2.txt", "Bible_3.txt", "Alice.txt", "Frankenstein.txt", "A-Christmas-Carol.txt"), r)
        //t.test(100, List("Bible_1.txt", "Bible_2.txt", "Bible_3.txt", "Alice.txt", "Frankenstein.txt", "A-Christmas-Carol.txt"), r)
        //t.test(250, List("Bible_1.txt", "Bible_2.txt", "Bible_3.txt", "Alice.txt", "Frankenstein.txt", "A-Christmas-Carol.txt"), r)
        t.test(500, List("Bible_1.txt", "Bible_2.txt", "Bible_3.txt", "Alice.txt", "Frankenstein.txt", "A-Christmas-Carol.txt"), r)
    }

    class EstJacc(){

        def test(n: Int, docs: List[String], range: List[Int]){
            var docs_shingles: mutable.Map[String, Set[String]] = mutable.Map()
            var text_a: mutable.Map[String, List[String]] = mutable.Map()
            var maps: mutable.Map[String, mutable.Map[String, Int]] = mutable.Map()
            var all_words: Set[String] = Set()

            var pw = new FileWriter("Est_Jacc" + n + ".txt") //save to file

            for(doc<-docs){ //reading all texts from file into array
                text_a += (doc -> TextReader(doc))
            }

            for(k<-4 to 13){ //changing shingle size
                pw.write("Shingle size: " + k + "\t n: " + n + "\n")

                for(doc<-docs){
                    docs_shingles += (doc -> Shingles(text_a(doc), k)) //get shingles for each document
                    all_words = all_words.union(docs_shingles(doc)) //get the set of all words from all documents
                }            

                //var hash_a = hashing(n, all_words) //hash functions

                for(doc<-docs){
                    maps += (doc -> Mapping(all_words, docs_shingles(doc))) //make dictionaries for each document
                }

                var sigs = Sig(maps, n, all_words) //get signatures
                EstJac(sigs, docs, pw) //calculate jaccard similarity between all documents

                docs_shingles.clear()
                maps.clear()
                all_words = Set()
            }
            pw.close()
        }

        def EstJac(signatures: mutable.ArrayBuffer[mutable.ArrayBuffer[Int]], doc: List[String], pw: FileWriter){
            var sub = Subsets(doc.length) //get all subsets (pairs) of documents
            var Jac: mutable.ArrayBuffer[Double] = mutable.ArrayBuffer()
            var j = 0

            for(perm<-sub){ //for each pair
                Jac.append(0.0)
                for(i<-0 to signatures.length - 1){
                    if(signatures(i)(perm(0)) == signatures(i)(perm(1))){ //check if signatures are the same
                        Jac(j) = Jac(j) + 1.0 //if yes increase jaccard by 1
                    }
                }

                Jac(j) = Jac(j) / signatures.length //normalize it

                println(doc(perm(0)) + "\t" + doc(perm(1)) + " \t " + Jac(j))
                pw.write(doc(perm(0)) + "\t" + doc(perm(1)) + " \t " + Jac(j) + "\n")
                j = j + 1
            }
        }

        def Subsets(documents: Int) = 0 to (documents - 1) combinations 2 //create all possible combinations of length 2 from set(1..# of documents)

        def Sig(maps: mutable.Map[String, mutable.Map[String, Int]], n: Int, all_words: Set[String]): mutable.ArrayBuffer[mutable.ArrayBuffer[Int]] = {
            var signatures: mutable.ArrayBuffer[mutable.ArrayBuffer[Int]] = mutable.ArrayBuffer()

            for(l<-0 to n - 1){ //go through every hash function we prepared
                var shuff = Random.shuffle(all_words.toSeq.toList)
                signatures.append(mutable.ArrayBuffer())
                for(doc<-maps.keys){ //go through every document

                    breakable{ //making it so we can use break in loop
                        for(i<-0 to shuff.length - 1){ //go thorugh every word in new order
                            if(maps(doc)(shuff(i)) == 1){ //find the first one that is in the document
                                signatures(l).append(i)
                                break
                            }
                        }
                    }
                }
            }

            return signatures
        }

        def Mapping(all_documents_words: Set[String], words_in_text: Set[String]): mutable.Map[String, Int] = {
            var B: mutable.Map[String, Int] = mutable.Map()

            for(word<-all_documents_words.toSeq){ //go through all shingles from all texts
                if(words_in_text.contains(word)){ //if the shingle occurs in current text, set shingle value to 1
                    B += (word -> 1)
                }
                else{ //otherwise set shingle value to 0
                    B += (word -> 0)
                }
            }

            return B
        }

        def hashing(n: Int, all_documents_words: Set[String]): mutable.ArrayBuffer[List[String]] = {
            var hash_array:  mutable.ArrayBuffer[List[String]] =  mutable.ArrayBuffer() //list containing hash functions
            var help_array = all_documents_words.toSeq.toList

            for(i<-0 to n-1){ //making n hash tables
                hash_array.append(Random.shuffle(help_array)) //append to hash array shuffled help arrary - one hash function
            }

            return hash_array
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