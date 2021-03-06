package dotty.tastydoc

import scala.tasty.Reflection
import dotty.tastydoc.references._

/** Trait containing methods for converting from Reflect types to References */
trait TastyTypeConverter {

  def convertTypeOrBoundsToReference(reflect: Reflection)(typeOrBounds: reflect.TypeOrBounds): Reference = {
    import reflect._

    def anyOrNothing(reference: Reference): Boolean = reference match {
      case TypeReference("Any", "/scala", _, _) => true
      case TypeReference("Nothing", "/scala", _, _) => true
      case _ => false
    }

    typeOrBounds match {
      case reflect.IsType(tpe) => convertTypeToReference(reflect)(tpe)
      case reflect.IsTypeBounds(reflect.TypeBounds(low, hi)) =>
        val lowRef = convertTypeToReference(reflect)(low)
        val hiRef = convertTypeToReference(reflect)(hi)
        if(hiRef == lowRef){
          hiRef
        }else{
          BoundsReference(lowRef, hiRef)
        }
      case reflect.NoPrefix() => EmptyReference
    }
  }

  def convertTypeToReference(reflect: Reflection)(tp: reflect.Type): Reference = {
    import reflect._

    //Inner method to avoid passing the reflection each time
    def inner(tp: reflect.Type): Reference = tp match {
      case reflect.Type.IsOrType(reflect.Type.OrType(left, right)) => OrTypeReference(inner(left), inner(right))
      case reflect.Type.IsAndType(reflect.Type.AndType(left, right)) => AndTypeReference(inner(left), inner(right))
      case reflect.Type.IsByNameType(reflect.Type.ByNameType(tpe)) => ByNameReference(inner(tpe))
      case reflect.Type.IsConstantType(reflect.Type.ConstantType(constant)) => ConstantReference(constant.value.toString)
      case reflect.Type.IsThisType(reflect.Type.ThisType(tpe)) => inner(tpe)
      case reflect.Type.IsAnnotatedType(reflect.Type.AnnotatedType(tpe, _)) => inner(tpe)
      case reflect.Type.IsTypeLambda(reflect.Type.TypeLambda(paramNames, paramTypes, resType)) => ConstantReference(tp.show(implicitly[reflect.Context].withoutColors)) //TOFIX
      case reflect.Type.IsRefinement(reflect.Type.Refinement(parent, name, info)) =>
        val tuple = convertTypeOrBoundsToReference(reflect)(info) match {
          case r if (info match {case reflect.IsTypeBounds(info) => true case _ => false}) => ("type", name, r)
          case r@TypeReference(_, _, _, _) => ("val", name, r)
          case ByNameReference(rChild) => ("def", name, rChild)
          case r => throw new Exception("Match error in info of Refinement. This should not happend, please open an issue. " + r)
        }
        convertTypeToReference(reflect)(parent) match {
          case RefinedReference(p, ls) =>
            RefinedReference(p, ls:+tuple)
          case t => RefinedReference(t, List(tuple))
        }
      case reflect.Type.IsAppliedType(reflect.Type.AppliedType(tpe, typeOrBoundsList)) =>
        inner(tpe) match {
          case TypeReference(label, link, _, hasOwnFile) =>
            if(link == "/scala"){
              if(label.matches("Function[1-9]") || label.matches("Function[1-9][0-9]")){
                val argsAndReturn = typeOrBoundsList.map(convertTypeOrBoundsToReference(reflect)(_))
                FunctionReference(argsAndReturn.take(argsAndReturn.size - 1), argsAndReturn.last, false)
              }else if(label.matches("Tuple[1-9]") || label.matches("Tuple[1-9][0-9]")){
                TupleReference(typeOrBoundsList.map(convertTypeOrBoundsToReference(reflect)(_)))
              }else{
                TypeReference(label, link, typeOrBoundsList.map(convertTypeOrBoundsToReference(reflect)(_)), hasOwnFile)
              }
            }else{
              TypeReference(label, link, typeOrBoundsList.map(convertTypeOrBoundsToReference(reflect)(_)), hasOwnFile)
            }
          case _ => throw Exception("Match error in AppliedType. This should not happen, please open an issue. " + tp)
        }
      case reflect.Type.IsTypeRef(reflect.Type.TypeRef(typeName, qual)) =>
        convertTypeOrBoundsToReference(reflect)(qual) match {
          case TypeReference(label, link, xs, _) => TypeReference(typeName, link + "/" + label, xs, true)
          case EmptyReference => TypeReference(typeName, "", Nil, true)
          case _ => throw Exception("Match error in TypeRef. This should not happen, please open an issue. " + convertTypeOrBoundsToReference(reflect)(qual))
        }
      case reflect.Type.IsTermRef(reflect.Type.TermRef(typeName, qual)) =>
        convertTypeOrBoundsToReference(reflect)(qual) match {
          case TypeReference(label, link, xs, _) => TypeReference(typeName + "$", link + "/" + label, xs)
          case EmptyReference => TypeReference(typeName, "", Nil)
          case _ => throw Exception("Match error in TermRef. This should not happen, please open an issue. " + convertTypeOrBoundsToReference(reflect)(qual))
        }
      case reflect.Type.IsSymRef(reflect.Type.SymRef(symbol, typeOrBounds)) => symbol match {
        case reflect.IsClassDefSymbol(_) => //Need to be split because these types have their own file
          convertTypeOrBoundsToReference(reflect)(typeOrBounds) match {
            case TypeReference(label, link, xs, _) => TypeReference(symbol.name, link + "/" + label, xs, true)
            case EmptyReference if symbol.name == "<root>" | symbol.name == "_root_" => EmptyReference
            case EmptyReference => TypeReference(symbol.name, "", Nil, true)
            case _ => throw Exception("Match error in SymRef/TypeOrBounds/ClassDef. This should not happen, please open an issue. " + convertTypeOrBoundsToReference(reflect)(typeOrBounds))
          }
        case reflect.IsTermSymbol(_) | reflect.IsTypeDefSymbol(_) =>
          convertTypeOrBoundsToReference(reflect)(typeOrBounds) match {
            case TypeReference(label, link, xs, _) => TypeReference(symbol.name, link + "/" + label, xs)
            case EmptyReference if symbol.name == "<root>" | symbol.name == "_root_" => EmptyReference
            case EmptyReference => TypeReference(symbol.name, "", Nil)
            case _ => throw Exception("Match error in SymRef/TypeOrBounds/Other. This should not happen, please open an issue. " + convertTypeOrBoundsToReference(reflect)(typeOrBounds))
          }
        case _ => throw Exception("Match error in SymRef. This should not happen, please open an issue. " + symbol)
      }
      case _ => throw Exception("No match for type in conversion to Reference. This should not happen, please open an issue. " + tp)
    }

    inner(tp)
  }
}
