/* 
    Intermediate Representation of functions and function calls in PScribble
    
    Peter Loftus
    12/9/11
*/

object PScribbleIntermediate {

// Case class for function definitions
case class Function(val name: String, val args:List[String], val body:List[FunctionCall]) {
    
    // Method that creates a string which can be parsed by the PScribbleParser
    def buildString(newArgs:List[String]):String = {
    
        var s = ""
        
        for (f <- this.body){
        
            //Start the line
            s += f.name + "("
            
            for (arg <- f.args){
                // Replace arg names with values if need be
                if (this.args.contains(arg)){
                    var index = this.args.indexOf(arg)
                    val newArg = newArgs(index)
                    s += newArg
                // Otherwise the arg doesn't need replacing
                } else 
                    s += arg
                // If we need to, put commas between args
                if (f.args.length > 1 && arg != f.args.last)
                    s += ", "
            }
            // End the line
            s += ")" + '\n'
        }
        
        return s
    }
}

// Case class for function calls used to contin function name and list of args
case class FunctionCall(val name: String, var args:List[String])

}
