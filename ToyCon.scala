import scala.util.parsing.combinator._
import java.io.{FileReader, InputStreamReader}
import scala.collection.mutable.{Stack,  HashMap}


class ToyConParser extends JavaTokenParsers {

 //toy concatenative language written in Scala
  
var quoteIndex = 0
def quoteName = { quoteIndex += 1 ;"quot"+ quoteIndex}

var qTable=new HashMap[Any,List[Any]]()

lazy val item:Parser[Any] = quotation|
"true" ^^^ true|"false" ^^^ false |
  stringLiteral ^^ (s => s.substring(1, s.length() - 1)) |
   """[a-zA-Z_0-9\+\-\*\<\>\=]+""".r ^^
    (s => try {s.toLong} catch {case ex: NumberFormatException => Symbol(s)})

lazy val quotation:Parser[Any]="[" ~> rep(item) <~ "]" ^^
		{script =>{var name =  Symbol("")  ;name=Symbol(quoteName); qTable += (name -> script);name}}
lazy val word:Parser[(Any,List[Any])]=":" ~>rep(item)<~ ";" ^^
		{case name::script => (name -> script)}
lazy val program:Parser[HashMap[Any,List[Any]]]=rep(word) ^^ {HashMap() ++= _ }

}


object ToyCon{

var dataStack= new Stack[Any] ()
var procStack= new Stack[Any] ()

var quoteTable=new HashMap[Any,List[Any]]()
var wordTable=new HashMap[Any,List[Any]]()


def pop=dataStack.pop
def push(v:Any)=dataStack.push(v)

   def ilB2(f:(Long,Long)=>Boolean){(pop,pop) match{
	  case (x,y)=>
    	dataStack.push(f(y.asInstanceOf[Long],x.asInstanceOf[Long]))
      }}

   def iArI2(f:(Long,Long)=>Long){(pop,pop) match{
	  case (x,y)=>
    	dataStack.push(f(y.asInstanceOf[Long],x.asInstanceOf[Long]))
      }}


def callWord(x:Any,y:HashMap[Any,List[Any]]){procStack++=y(x).reverse}



 val execTable:HashMap[Any,()=>Unit]= HashMap(

		(Symbol("drop"),  ()=>{ pop }),
		(Symbol("dup"),  ()=>{val v= pop;push(v);push(v) }),
	 	(Symbol("swap"),()=>{(pop,pop) match {case (x,y) => {push(x);push(y)}}}),
		(Symbol("rot"),()=>{(pop,pop,pop) match {case (x,y,z) => {push(x);push(z);push(y)}}}),
		(Symbol("if"),()=>{(pop,pop,pop) match {case (x,y,z) =>{if(z==true) callWord(y,quoteTable) else callWord(x,quoteTable)}}}),
		(Symbol("dip"),()=>{(pop,pop) match {case (x,y) => {procStack.push(y);callWord(x,quoteTable)}}}),

	    (Symbol("+"),() => {iArI2(_+_)}),
	    (Symbol("-"),()=> {iArI2(_-_)} ),
	    (Symbol("*"),()=> { iArI2(_*_)} ),
	    (Symbol("/"),()=> { iArI2(_/_)} ),
	    (Symbol(">"),()=> { ilB2(_>_) } ),
	    (Symbol("<"),()=> { ilB2(_<_) } ),
	    (Symbol(">="),()=> { ilB2(_>=_)} ),
	    (Symbol("<="),()=> { ilB2(_<=_) } ),
	    (Symbol("=="),()=> { ilB2(_==_) } )

	 	)

def displayStack{   println("dataStack:"+dataStack.toString)
					println("procStack:"+procStack.toString)}



def main(args: Array[String]) {
val reader = if (args(0) == "-") new InputStreamReader(System.in)
else new FileReader(args(0))
val parsers = new ToyConParser
 wordTable = parsers.parseAll(parsers.program, reader).get
 quoteTable=parsers.qTable


 def evalProcTop{
	 procStack.pop match {

		 case x if quoteTable.contains(x) => {push(x)}
		 case x if wordTable.contains(x) => {callWord(x,wordTable)}
		case x if execTable.contains(x) => {execTable(x) () }
		 case x => push(x)
			     }

		}

def evalProcAll{ while( ! procStack.isEmpty ){displayStack;evalProcTop} }

println(wordTable)
println(quoteTable)



callWord(Symbol("main"),wordTable)
evalProcAll

displayStack


}

}

