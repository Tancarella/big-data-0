import scala.collection._
import scala.collection.mutable.ListMap

object Main{

  def main(args: Array[String]){
    var edges: mutable.Map[Int, List[Int]] = mutable.Map()
    edges += (1 -> List(2,3), 3 -> List(1, 5), 2 -> List(5), 5 -> List()) //original map of edges
    println(edges)

    var inverted: mutable.Map[Int, List[Int]] = mutable.Map() //empty map to keep inverted edges

    var flip = new FlipEdges(edges, inverted) //make a new class
    flip.MapReduce() //do Map-Reduce
    flip.ShowInverted() //show inverted edges
  }
}

class FlipEdges(var edges_map: mutable.Map[Int, List[Int]], var inverted_edges: mutable.Map[Int, List[Int]]){

  def ShowInverted(){
    println(inverted_edges) //printing inverted map of edges
  }

  def MapReduce(){
    for(i<-edges_map.keys){ //for every key in original list of edges
      FReduce(FMap(i, edges_map(i))) //call reduce function that will also call the map function
    }
  }

  def FMap(key: Int, values: List[Int]): mutable.Map[Int, Int] ={
    var nMap: mutable.Map[Int, Int] = mutable.Map() //new empty map keeping reversed edges

    for (value<-values){ //go through each value in the array
      nMap += (value -> key) //add to map pairs (value, key)
    }

    return nMap //return map of inversed edges
  }

  def FReduce(map_add: mutable.Map[Int, Int]){
    for (i<-map_add.keys){ //go through every key in the map of edges we want to add

      if(inverted_edges.contains(i)){ //if our end-map contains the key, append the array to contain new value
        inverted_edges(i) = inverted_edges(i) :+ map_add(i)
      }

      else{ //map doesnt contain the key, create a new entry
        inverted_edges += (i -> List(map_add(i)))
      }
    }
  }
}
