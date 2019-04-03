package dotty.tastydoc

import scala.tasty.Reflection
import scala.tasty.file.TastyConsumer

import dotty.tools.dotc.tastyreflect
import scala.collection.mutable.HashMap
import scala.collection.mutable.Set
import scala.annotation.tailrec

import java.io._

class TastydocConsumer extends TastyConsumer {
  final def apply(reflect: Reflection)(root: reflect.Tree): Unit = {
    import reflect._
    import internal._
    import references._

    println("Full tree =========================")
    println(root.show)
    println("End of full tree ==================")

    def traverse(child: reflect.Tree) : Container = {

      def beautifyType(typeTree: reflect.TypeTree) : String = {
        typeTree.tpe.showCode
        .replaceAll("\u001B\\[[;\\d]*m", "") //TODO: Other way to remove color?
        //.stripPrefix("scala.")
      }

      def beautifyImport(expr: reflect.Term, selectors: List[reflect.ImportSelector]) : String = {
        expr.showCode.replaceAll("\u001B\\[[;\\d]*m", "") +
        "." +
        selectors
      }

      def extractUserDoc(comment: Option[reflect.Comment]) : String = comment match {
        case Some(com) => com.raw
        case None => ""
      }

      child match {
        //case reflect.Term.Ident(name) => name
        case reflect.PackageClause(pid, stats) =>
          //"Package: "+ traverse(level, pid) + "\n" +
          //stats.map(traverse(level+1, _)).foldLeft("")(_+_)
          new PackageContainer("package " + pid, stats.map(traverse(_)), extractUserDoc(child.symbol.comment)) //TODO: Optional new?

        case reflect.Import(impliedOnly, expr, selectors) =>
          new ImportContainer("import " + beautifyImport(expr, selectors), extractUserDoc(child.symbol.comment))

        case reflect.ClassDef(name, constr, parents, derived, self, body) =>
          //TODO: Generic type
          //TODO: TypeDef
          //TODO: Classes inside class
          val sign = "class: " + name
          def iterBody(body: List[reflect.Statement], defdef: List[Container], valdef: List[Container], typedef: List[Container]) : (List[Container], List[Container], List[Container]) = body match {
            case Nil => (defdef.reverse, valdef.reverse, typedef.reverse) //TODO: More efficient than reverse?
            case (x @ reflect.DefDef(_, _, _, _, _)) :: xs => iterBody(xs, traverse(x)::defdef, valdef, typedef)
            case (x @ reflect.ValDef(_, _, _)) :: xs => iterBody(xs, defdef, traverse(x)::valdef, typedef)
            //case (x @ reflect.TypeDef())
            case x :: xs => iterBody(xs, defdef, valdef, typedef)
          }
          val (defdef, valdef, typedef) = iterBody(body, Nil, Nil, Nil)
          new ClassContainer(sign, defdef, valdef, typedef, extractUserDoc(child.symbol.comment))

        case reflect.DefDef(name, typeParams, paramss, tpt, rhs) =>
          @tailrec def handleParams(ls: List[List[ValDef]], str: String) : String = ls match {
            case Nil => str
            case List()::xs => handleParams(xs, str + "()")
            case args::xs => handleParams(xs, str + "(" + args.map{case ValDef(vname, vtype, _) => vname + ": " + beautifyType(vtype)}.reduce((x, y) => x + ", " + y) + ")")
          }
          new DefContainer("def " +
            name +
            handleParams(paramss, "") +
            " : " +
            beautifyType(tpt),
            extractUserDoc(child.symbol.comment)
          )

        case reflect.ValDef(name, tpt, rhs) =>
          new ValContainer("val " +
            name +
            " : " +
            beautifyType(tpt),
            extractUserDoc(child.symbol.comment)
          )

        case _ => new MissingMatchContainer()
      }
    }

    def convertToEntity(child: reflect.Tree) : Entity = child match {
        case reflect.PackageClause(pid, stats) =>
          //Find whole package path
          if(!stats.isEmpty && false){ //TOASK: isInstanceOf not working? find package recursively is right?
            convertToEntity(stats.head)
          } else{
            val pidSplit = pid.symbol.showCode.split("\\.") //TOASK: showcode on symbol?
            //TODO: Complete
            val annotations = Nil
            val name = pidSplit.last //TOASK: Cleaner directly in constructor?
            val members = stats.map(convertToEntity)
            val path = pidSplit.init.toList
            val superTypes = Nil
            val comment = None
            val parent = None
            PackageImpl(annotations, name, members, path, superTypes, comment, parent)
          }

        case reflect.Import(impliedOnly, expr, selectors) =>
          //TODO: rework all
          val annotations = Nil
          val name = selectors.map(_.toString).reduce(_+_)
          val path = expr.symbol.showCode.split("\\.").toList
          val comment = None
          val parent = None
          ImportImpl(annotations, name, path, comment, parent)

        case reflect.ClassDef(name, constr, parents, derived, self, body) =>
          //TODO: Generic type
          //TODO: TypeDef
          //TODO: Classes inside class
          //TODO: case class

          //child.symbol.annots.foreach(x => println(x.symbol.showCode))
          //TODO: Complete
          val annotations = Nil
          val members = body.map(convertToEntity)
          val modifiers = child.symbol.flags.showCode.replaceAll("\\/\\*|\\*\\/", "").split(" ").toList
          val path = Nil
          // typeParams: List[String] = Nil,
          // constructors: List[List[ParamList]] = Nil,
          // superTypes: List[MaterializableLink] = Nil,
          // var comment: Option[Comment] = None,
          // var companionPath: List[String] = Nil,
          // var parent: Option[Entity] = None
          // new ClassContainer(sign, defdef, valdef, typedef, extractUserDoc(child.symbol.comment))
          ClassImpl(annotations, name, members, modifiers, path)

        case reflect.ValDef(name, tpt, rhs) =>
          //println(tpt.showCode)
          //TODO: Complete
          val annotations = Nil
          val modifiers = Nil
          val path = Nil
          val returnValue = null
          val kind = null
          // var comment: Option[Comment] = None,
          // implicitlyAddedFrom: Option[Reference] = None,
          // var parent: Option[Entity] = None
          ValImpl(annotations, name, modifiers, path, returnValue, kind)

        case _ => EmptyPackage(Nil, "Error") //TODO: should never go there
    }

    //print(formatToMarkdown(traverse(root), 0))
    // val pw = new PrintWriter(new File("./tastydoc/docOutputTest.md" ))
    // pw.write(formatToMarkdown(traverse(root), 0))
    // pw.close()

    println("Start convert to Representation")

    //convertToEntity(root)
    println(formatRepresentationToMarkdown(representations.convertToRepresentation(reflect)(root), false))

    val pw = new PrintWriter(new File("./tastydoc/docOutputTest.md" ))
    pw.write(formatRepresentationToMarkdown(representations.convertToRepresentation(reflect)(root), false))
    pw.close()

    println("Start comment parsing")

  }
}