/*
    Parser for PScribble programs
    
    Peter Loftus
    12/9/11
    
    A valid program in PScribble is any number of rules, each on a new line
    
    A rule can be an operator or a function
    
    Operators are pre-defined by Image Magick, not all are implemented here
    
    Functions can be definitions or calls
    
    Definitions have the form:
    def name (args) {body}
    
    Calls have the form 
    name(args)
    
    name can be any string of upper case or lower case letters, numbers, and _
    
    args is a list of any length of strings that can contain upeer or lower
    case letters and numbers args can also take the form of Image Magick input
    
    body is any number of function or operator calls each on a new line
*/

import scala.util.parsing.combinator._
import PScribbleIntermediate._

object PScribbleParser {

class PScribbleParser extends JavaTokenParsers {
    
    def program:    Parser[List[Any]] = rep1sep(rule, '\n')
    def rule:       Parser[Any] = operator | function
    def operator:   Parser[String] = (resize
                                    | scale
                                    | enhance
                                    | sepia
                                    | solarize
                                    | adap_resize
                                    | auto_gamma
                                    | auto_level
                                    | threshold
                                    | b_threshold
                                    | w_threshold
                                    | blue_shift
                                    | charcoal
                                    | emboss
                                    | equalize
                                    | flip
                                    | flop
                                    | implode
                                    | magnify
                                    | monochrome
                                    | motion_blur
                                    | negate
                                    | normalize
                                    | paint
                                    | polaroid
                                    | raise
                                    | sample
                                    | sharpen
                                    | sketch
                                    | swirl
                                    | transpose
                                    | u_colors
                                    | wave)
                                    
    def function:   Parser[Any] = fDef | fCall
    def fCall:      Parser[FunctionCall] = name~"("~args~")" ^^ {case n~"("~a~")" => new FunctionCall(n,a)}                             
    def fDef:       Parser[Function] = "def "~name~"("~variables~")"~"{"~body~"}" ^^ {case "def "~n~"("~a~")"~"{"~b~"}" => new Function(n, a, b)}
    def name:       Parser[String] = """[[a-zA-Z0-9]|\_]+""".r
    def variable:   Parser[String] = """[a-zA-Z0-9]*""".r
    def arg:        Parser[String] = geometry | value | variable
    def variables:  Parser[List[String]] = rep1sep(variable, ',')
    def args:       Parser[List[String]] = rep1sep(arg, ',')
    def body:       Parser[List[FunctionCall]] = rep1sep(fCall, '\n')
                                    
    def resize:     Parser[String] = "resize("~geometry~")" ^^ {case "resize("~g~")" => "-resize "+g}
    def scale:      Parser[String] = "scale("~geometry~")" ^^ {case "scale("~g~")" => "-scale "+g}
    def enhance:    Parser[String] = "enhance()" ^^ {case s => "-enhance"}
    def sepia:      Parser[String] = "sepia-tone("~percent~")" ^^ {case "sepia-tone("~t~")" => "-sepia-tone "+t}
    def solarize:   Parser[String] = "solarize("~percent~")" ^^ {case "solarize("~t~")" => "-solarize "+t}
    def adap_resize:Parser[String] = "adaptive_resize("~geometry~")" ^^ {case "adaptive_resize("~g~")" => "-adaptive-resize "+g}
    def auto_gamma: Parser[String] = "auto_gamma()" ^^ {case s => "-auto-gamma"}
    def auto_level: Parser[String] = "auto_level()" ^^ {case s => "-auto-level"}
    def threshold:  Parser[String] = "threshold("~channel~","~percent~")" ^^ {case "threshold("~c~","~v~")" => "-channel "+c+" -threshold "+v}
    def b_threshold:Parser[String] = "black_threshold("~percent~")" ^^ {case "black_threshold("~t~")" => "-black-threshold "+t}
    def w_threshold:Parser[String] = "white_threshold("~percent~")" ^^ {case "white_threshold("~t~")" => "-white-threshold "+t}
    def blue_shift: Parser[String] = "blue_shift("~value~")" ^^ {case "blue_shift("~v~")" => "-blue-shift "+v}
    def charcoal:   Parser[String] = "charcoal("~value~")" ^^ {case "charcoal("~v~")" => "-charcoal "+v}
    def emboss:     Parser[String] = "emboss("~value~")" ^^ {case "emboss("~v~")" => "-emboss "+v}
    def equalize:   Parser[String] = "equalize()" ^^ {case s => "-equalize"}
    def flip:       Parser[String] = "flip()" ^^ {case s => "-flip"}
    def flop:       Parser[String] = "flop()" ^^ {case s => "-flop"}
    def implode:    Parser[String] = "implode("~value~")" ^^ {case "implode("~v~")" => "-implode "+v}
    def magnify:    Parser[String] = "magnify("~value~")" ^^ {case "magnify("~v~")" => "-magnify "+v}
    def monochrome: Parser[String] = "monochrome()" ^^ {case s => "-monochrome"}
    def motion_blur:Parser[String] = "motion_blur("~value~")" ^^ {case "motion_blur("~v~")" => "-motion-blur "+v}
    def negate:     Parser[String] = "negate()" ^^ {case s => "-negate"}
    def normalize:  Parser[String] = "normalize()" ^^ {case s => "-normalize"}
    def paint:      Parser[String] = "paint("~value~")" ^^ {case "paint("~v~")" => "-paint "+v}
    def polaroid:   Parser[String] = "polaroid("~value~")" ^^ {case "polaroid("~v~")" => "-polaroid "+v}
    def raise:      Parser[String] = "raise("~value~")" ^^ {case "raise("~v~")" => "-raise "+v}
    def sample:     Parser[String] = "sample("~geometry~")" ^^ {case "sample("~g~")" => "sample "+g}
    def sharpen:    Parser[String] = "sharpen("~value~")" ^^ {case "sharpen("~v~")" => "-sharpen "+v}
    def sketch:     Parser[String] = "sketch("~value~")" ^^ {case "sketch("~v~")" => "-sketch "+v}
    def swirl:      Parser[String] = "swirl("~value~")" ^^ {case "swirl("~v~")" => "-swirl "+v}
    def transpose:  Parser[String] = "transpose()" ^^ {case s => "-transpose"}
    def u_colors:   Parser[String] = "unique_colors()" ^^ {case s => "-unique-colors"}
    def wave:       Parser[String] = ("wave("~value~")" ^^ {case "wave("~v~")" => "-wave "+v}
                                    | "wave("~value~","~value~")" ^^ {case "wave("~v1~","~v2~")" => "-wave "+v1+"x"+v2})
    
    def geometry:   Parser[String] = percent | dims
    def dims:       Parser[String] = (value~"x"~value~"^" ^^ {case x~"x"~y~"^" => x+"x"+y+"^"}
                                    | value~"x"~value~"!" ^^ {case x~"x"~y~"!" => x+"x"+y+"!"}
                                    | value~"x"~value~">" ^^ {case x~"x"~y~">" => x+"x"+y+">"}
                                    | value~"x"~value~"<" ^^ {case x~"x"~y~"<" => x+"x"+y+"<"}
                                    | value~"x"~value ^^ {case x~"x"~y => x+"x"+y}
                                    | "x"~value ^^ {case "x"~h => "x"+h}
                                    | value~"@" ^^ {case a~"@" => a+"@"}
                                    | value ^^ {case x => x})
    def percent:  Parser[String] = value~"%" ^^ {case x~"%" => x+"%"}   
    def value:    Parser[String] = decimalNumber
    def channel:  Parser[String] =  "All" | "RGBA" | "RGB" | "CMYKA" | "CMYK" | "R" | "G" | "B" | "A" | "O" | "C" | "M" | "Y" | "K" 
}
}
