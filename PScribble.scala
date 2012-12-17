/*
    Main Object for the PScribble DSL
    
    Peter Loftus
    12/9/11
    
    to build, run: scalac -d build *.scala
    
    to run: 
    ./ps <input file> <input image> <output image>
    or:
    scala -classpath build PScribble <input file> <input image> <outpuf file>
    
    note: build can be replaced with your choice of directory
    
    A list of ImageMagick commands with their accepted inputs can be found here:
    http://www.imagemagick.org/script/convert.php
*/

import PScribbleParser._
import PScribbleIntermediate._

object PScribble {


    //Variables:
    
    // List to keep track of all user-defined functions
    var definedFunctions:List[Function] = Nil
    
    // List used to check for recursive calls, which aren't allowed
    var callingFunctions:List[String] = Nil
    
    // List containing all of the image magick operators
    val operators:List[String] = List("resize", "scale", "enhance", "sepia",
                                  "solarize", "adaptive_resize", "auto_gamma",
                                  "auto_level", "threshold", "black_threshold",
                                  "white_threshold", "blue_shift", "charcoal",
                                  "emboss", "equalize", "flip", "flop", "implode",
                                  "magnify", "monocrhome", "motion_blur", "negate",
                                  "normalize", "paint", "polariod", "raise",
                                  "sharpen", "sketch", "swirl", "transpose",
                                  "unique_colors", "wave")
                                  
    //Methods:
    
    // Method to execute a string in the cmd line
    def exec(cmd: String) = Runtime.getRuntime exec cmd    
    
    // Method that throws an exception
    def except(err: String, function: String) = {
        
        // Variables to be thrown in the exception
        var msg:String = ""
        var cause:Throwable = new Throwable("")
        
        // Depending on the input error, give a different message and cause
        if (err == "defined name"){
            msg = "functions must have distinct names."
            cause = new Throwable("function "+'"'+function+'"'+
                                  " is defined more than once.")
        }
        else if (err == "operator"){
            msg = "misuse of pre-defined operator."
            cause = new Throwable("input type for "+'"'+function+'"'+
                                  " is incorrect, check the documentation"+
                                  " for proper use")
        }
        else if (err == "undefined name"){
            msg = "function used before its definition."
            cause = new Throwable("function "+'"'+function+'"'+
                                  " is either called before its definition"+
                                  " or it has no definition")
        }
        else if (err == "recursion"){
            msg = "infinite recursion."
            cause = new Throwable("function: "+'"'+function+'"'+
                                  " calls itself directly or indirectly.")
        }
        else if (err == "parser"){
            msg = "error in parsing."
            cause = new Throwable("Syntax error in the program")
        }
        
        // Finally, throw the exception
        throw new Exception(msg, cause)                                 
    }
    
    // Method which parses a program in the form of a string
    def parse(program: String):String = {
    
        // Create the parser and run it on the program input to the function
        val parser = new PScribbleParser()
        val result = parser.parseAll(parser.program, program)
        
        // String to be returned
        var s1 = ""
        
        // If the parser fails, print the message it gives and then stop
        if (!result.successful){
            println(result)
            except("parser", "")
        }
        
        // If the parser succeeds, convert the intermediate representation into
        // a callable cmd line string
        if (result.successful) {
        
            // Step through each line of the input program
            for (i <- result.get){
                i match {                
                    // If i is a function definition:
                    case i : Function => {                    
                        // If i's name has already been used, throw the
                        // defined name exception
                        if (definedFunctions.exists(_.name == i.name))
                            except("defined name", i.name)
                        
                        // Add i to the list of defined functions
                        else
                            definedFunctions = i::definedFunctions}
                            
                    // If i is a function call:
                    case i : FunctionCall => {                    
                        // Try to find it in defined functions
                        val f = definedFunctions.find(_.name == i.name)
                        
                        // If it isn't there:
                        if (f.isEmpty){
                            // And it is a pre-defined operator, throw the 
                            // operator exception
                            if (operators.exists(_ == i.name))
                                except("operator", i.name)
                                
                            // Otherwise throw the undefined name exception
                            else 
                                except("undefined name", i.name)
                                
                        // Otherwise i has been defined already:
                        } else {                        
                            // If i has already been called in this call path,
                            // throw the recursion exception
                            if (callingFunctions.exists(_ == i.name))
                                except("recursion", i.name)
                            
                            // Add i to a list of functions representing 
                            // the call path
                            callingFunctions = i.name::callingFunctions
                            
                            // Get the actual function from the Option
                            val j = f.get
                            // Turn the function into a string that can be 
                            // recursively parsed
                            val x = j.buildString(i.args)
                            // Parse the string
                            val y = parse(x)
                            
                            // Remove i from the call path since we're leaving
                            // its scope
                            callingFunctions = callingFunctions.drop(1)
                            
                            // Add the returned string to the return string
                            s1 += y
                        }                        
                    }
                                        
                    // If i is just a string, it's an operation call and
                    // already in the right format, so just add it in
                    case i : String => s1 += i + " "}
            }
        }
        
        // Return the competed string
        return s1
    }
    
    // Main Method, called from the command line
    def main(args: Array[String]): Unit = {
    
        // Using exception handling
        try {
        
        // Make a string from the input program file
        val program = io.Source.fromFile(args(0)).mkString 
        
        // String that will eventually be called on the command line
        var s = "convert "  
        
        // Add the input image to s
        s += args(1) + " " 
        
        // Parse the program and add the string returned to s
        s += parse(program) 
        
        // Add the output image to s
        s += args(2)
        
        // Print out the image magick command being called
        System.out.println("calling: " + s)
        
        // Call the image magick command
        exec(s)
        
        } catch {
        
        // Catch any exception thrown, then print its message and cause
        case e => println("program failed with error: " + e.getMessage() + 
                          "\ncause: "+e.getCause().getMessage())   
        }
    }
}
