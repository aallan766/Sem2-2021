/**
 * HasLang syntax analyser.
 *
 * Copyright 2021, Anthony Sloane, Matthew Roberts, Kym Haines, Macquarie University, All rights reserved.
 */

package haslang

import org.bitbucket.inkytonik.kiama.util.Positions
import java.util.ListIterator
import org.bitbucket.inkytonik.kiama.parsing.Parsers

/**
 * Module containing parsers for HasLang.
 */
class SyntaxAnalysis (positions : Positions) extends Parsers (positions) {

    import HasLangTree._
    import scala.language.postfixOps

    lazy val parser : PackratParser[Program] =
        phrase (program)

    lazy val program : PackratParser[Program] =
        exp ^^ Program

    lazy val literal : PackratParser[Exp] =
        "false" ^^^ BoolExp (false) |
        "true" ^^^ BoolExp (true) |
        integer ^^ (s => IntExp (s.toInt)) 
  
    
    lazy val factor : PackratParser[Exp] =
        lam |
        literal |
        exp |
        identifier ^^ IdnUse |
        "(" ~> exp <~ ")" |
        tupleExp | 
        failure ("exp expected")

    // add parsers between factor and exp
    lazy val tupleExp : PackratParser[TupleExp] =
        "(" ~> (rep1sep(factor, ",")) <~ ")" ^^ {case t => TupleExp(t)} 

    lazy val lam : PackratParser[LamExp] = 
        ("\\" ~> idndef) ~ ("->" ~> exp) ^^ {case idn ~ body => LamExp(idn, body)} 

    lazy val listExp : PackratParser[ListExp] = 
        "[" ~ "]" ^^^ ListExp(Vector()) | 
        "[" ~> repsep(factor, ",") <~ "]" ^^ {case t => ListExp(t)}


    lazy val exp : PackratParser[Exp] =
        ("if" ~> exp) ~ ("then" ~> exp) ~ ("else" ~> exp) ^^ {
            case cond ~ left ~ right => IfExp (cond, left, right)
        } |
        ("let" ~> definitions) ~ ("in" ~> exp) ^^ {case e ~ f => LetExp(e, f)} |
        exp ~ factor ^^ {case e ~ f => AppExp(e, f)} |
        exp ~ ("==" ~> factor) ^^ {case e ~ f => EqualExp(e, f)} | 
        exp ~ ("<" ~> factor) ^^ {case e ~ f => LessExp(e, f)} | 
        exp ~ ("+" ~> factor) ^^ {case e ~ f => PlusExp(e, f)} |
        exp ~ ("-" ~> factor) ^^ {case e ~ f => MinusExp(e, f)} |
        exp ~ ("*" ~> exp) ^^ {case e ~ f => StarExp(e, f)} | 
        exp ~ ("/" ~> exp) ^^ {case e ~ f => SlashExp(e, f)} |
        exp ~ (":" ~> factor) ^^ {case e ~ f => ConsExp(e, f)} |
        identifier ^^ {case e => IdnUse(e)} |
        listExp |
        factor  

    lazy val definitions : PackratParser[Vector[Defn]] =
        rep1sep(defn, ";") ^^ Vector[Defn]

    lazy val defn : PackratParser[Defn] =
        idndef ~ rep1sep(funline, ".") ^^ Defn

    lazy val funline : PackratParser[FunLine] =
       (identifier?) ~ ((pat*) <~ "=") ~ exp ^^ {case id ~ pat ~ exp => {
            id match {
                case None => FunLine("", pat, exp)
                case _ => FunLine(id.get, pat, exp)
            }
        }}

    lazy val pat : PackratParser[Pat] =
        basicpat ~ (":" ~> pat) ^^ {case b ~ p => ConsPat(b, p)} |
        "[" ~> repsep(pat, ",") <~ "]" ^^ {case t => ListPat(t)} | 
        "(" ~> rep1sep(pat, ",") <~ ")" ^^ {case t => TuplePat(t)} |
        basicpat

    lazy val basicpat : PackratParser[Pat] =
        literal ^^ LiteralPat |
        "_" ^^^ AnyPat() | 
        identifier ^^ IdentPat |
        "(" ~> pat <~ ")"

    lazy val tipe : PackratParser[Type] =
        tipe ~ ("->" ~> tipe) ^^ {case tipe1 ~ tipe2 => FunType(tipe1,tipe2)} |
        ("[" ~> tipe <~ "]") ^^ {case t => ListType(t)} |
        basictipe | 
        "(" ~> rep1sep(tipe, ",") <~ ")" ^^ {case t => TupleType(t)} 
        
    lazy val basictipe : PackratParser[Type] =
        "Int" ^^^ IntType() |
        "(" ~ ")" ^^^ UnitType() |
        "Bool" ^^^ BoolType() |
        "(" ~> tipe <~ ")" 

    // NOTE: You should not change anything below here...

    lazy val integer =
        regex ("[0-9]+".r)

    lazy val idndef =
        (identifier <~ "::") ~ tipe ^^ IdnDef

    val keywordStrings =
        List ("let", "else", "false", "if", "then", "true", "in")

    lazy val keyword =
        keywords ("[^a-zA-Z0-9_]".r, keywordStrings)

    lazy val identifier =
        not (keyword) ~> identifierBase

    lazy val identifierBase =
        regex ("[a-zA-Z][a-zA-Z0-9_]*".r) |
        failure ("identifier expected")

    lazy val whitespaceParser : PackratParser[Any] =
        rep ( """\s""".r | comment)

    lazy val comment : PackratParser[Any] =
        "{-" ~ rep (not ("-}") ~ (comment | any)) ~ "-}" |
        "--.*(\n|\\z)".r

}
