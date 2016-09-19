package es.weso.shex.compact
import es.weso.shex._
import es.weso.rdf.nodes._
import es.weso.shex.parser._
import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree._
import collection.JavaConverters._
import cats._, data._
import cats.implicits._

object Parser {

  type Builder[A] = EitherT[S,String,A]
  case class BuilderState(prefixMap: Map[Prefix,IRI], base: Option[IRI])
  type S[A] = State[BuilderState,A]

  def initialState = BuilderState(Map(),None)

  def ok[A](x: A): Builder[A] =
    EitherT.pure(x)

  def err[A](msg: String): Builder[A] =
      EitherT.left(StateT.pure(msg))

  def getState: Builder[BuilderState] =
    EitherT.liftT(StateT.inspect(identity))

  def getPrefixMap: Builder[Map[Prefix,IRI]] = for {
      s <- getState
    } yield s.prefixMap

  def getBase: Builder[Option[IRI]] = for {
      s <- getState
    } yield s.base

  def addBase(base: IRI): Builder[Unit] = {
    EitherT.liftT(
      StateT.modify(
        s => s.copy(base = Some(base))))
  }

  def run[A](c: Builder[A]):( BuilderState, Either[String,A]) = c.value.run(initialState).value

  def addPrefix(prefix:Prefix,iri: IRI): Builder[Unit] = {
    EitherT.liftT(
      StateT.modify(s =>
        s.copy(prefixMap = s.prefixMap + (prefix -> iri))))
  }

  def parseSchema(str: String): Either[String,Schema] = {
    val input: ANTLRInputStream = new ANTLRInputStream(str)
    val lexer: ShExDocLexer = new ShExDocLexer(input)
    val tokens: CommonTokenStream = new CommonTokenStream(lexer)
    val parser: ShExDocParser = new ShExDocParser(tokens)
    val maker = new SchemaMaker()
    val builder = maker.visit(parser.shExDoc()).asInstanceOf[Builder[Schema]]
    run(builder)._2
  }

 class SchemaMaker extends ShExDocBaseVisitor[Any] {

   override def visitShExDoc(
     ctx: ShExDocParser.ShExDocContext): Builder[Schema] = {
    for {
      directives <- {
        val r: List[Builder[Directive]] =
          ctx.directive().asScala.map(visitDirective(_)).toList
        r.sequence
      }
      startActions <- visitStartActions(ctx.startActions())
      prefixMap <- getPrefixMap
      base <- getBase
    } yield {
      Schema.empty.copy(
        prefixes = if (!prefixMap.isEmpty) Some(prefixMap) else None,
        base = base,
        startActs = startActions
      )
    }
/*     for {
       startDirectives <-
       notStartAction <-
         if (isDefined(ctx.notStartAction())) {
         visitNotStartAction(ctx.notStartAction())
         } else
         ???
    } yield Schema.empty.copy(
           prefixes = if (prefixes.isEmpty) None else Some(prefixes),
           base = base
         )
    }  */
   }

   type Start = Option[(ShapeExpr, List[SemAct])]
   type NotStartAction = Either[Start,(ShapeLabel,ShapeExpr)]

   override def visitNotStartAction(
     ctx: ShExDocParser.NotStartActionContext
   ): Builder[NotStartAction] = {
     if (isDefined(ctx.start())) {
       for {
         s <- visitStart(ctx.start())
       } yield Left(s)
     } else {
       for {
        s <- visitShape(ctx.shape())
      } yield Right(s)
     }
   }


   override def visitStartActions(ctx: ShExDocParser.StartActionsContext): Builder[Option[List[SemAct]]] = {
     if (isDefined(ctx)) {
         val r: List[Builder[SemAct]] =
           ctx.codeDecl().asScala.map(visitCodeDecl(_)).toList
         r.sequence.map(Some(_))
     } else ok(None)
   }

   override def visitCodeDecl(
    ctx: ShExDocParser.CodeDeclContext): Builder[SemAct] =
      for {
       iri <- visitIri(ctx.iri())
     } yield {
      val code: Option[String] =
        if (isDefined(ctx.CODE()))
          Some(ctx.CODE().getText())
        else
          None
      SemAct(iri,code)
    }

/*     if (ctx.start() != null) {
       Left(visitStart(ctx.start()))
     } else {
       Right(visitShape(ctx.shape()))
     } */

   override def visitStart(
     ctx: ShExDocParser.StartContext):
       Builder[Option[(ShapeExpr,List[SemAct])]] = {
     if (isDefined(ctx)) {
       ???
     } else
       ok(None)
   }

   override def visitShape(ctx: ShExDocParser.ShapeContext): Builder[(ShapeLabel,ShapeExpr)] =
    for {
     label <- visitShapeLabel(ctx.shapeLabel())
     shapeExpr <- obtainShapeExpr(ctx)
   } yield (label,shapeExpr)

   def obtainShapeExpr(ctx: ShExDocParser.ShapeContext): Builder[ShapeExpr] =
     if (isDefined(ctx.KW_EXTERNAL())) {
       ok(ShapeExternal()) // TODO: What happens if there are semantic actions after External??
     } else
      // TODO: Obtain stringFacet*
       visitShapeExpression(ctx.shapeExpression())

   override def visitShapeExpression(
     ctx: ShExDocParser.ShapeExpressionContext):
       Builder[ShapeExpr] =
         visitShapeDisjunction(ctx.shapeDisjunction())

   override def visitShapeDisjunction(
           ctx: ShExDocParser.ShapeDisjunctionContext):
             Builder[ShapeExpr] = for {
      shapes <- {
        val r: List[Builder[ShapeExpr]] =
          ctx.shapeConjunction().asScala.map(visitShapeConjunction(_)).toList
        r.sequence
      }
   } yield if (shapes.length == 1) shapes.head
        else ShapeOr(shapes)

   override def visitShapeConjunction(
           ctx: ShExDocParser.ShapeConjunctionContext):
             Builder[ShapeExpr] = { for {
     shapes <- {
       val r: List[Builder[ShapeExpr]] =
         ctx.negShapeAtom().asScala.map(visitNegShapeAtom(_)).toList
       r.sequence
     }
   } yield if (shapes.length == 1) shapes.head
        else ShapeOr(shapes)
   }

   override def visitNegShapeAtom(
           ctx: ShExDocParser.NegShapeAtomContext):
             Builder[ShapeExpr] = for {
    shapeAtom <- visitShapeAtom(ctx.shapeAtom())
   } yield shapeAtom // TODO: Handle negation

   override def visitShapeAtom(
     ctx: ShExDocParser.ShapeAtomContext):
       Builder[ShapeExpr] = {
    ctx match {
      case s: ShExDocParser.ShapeAtomLiteralContext => ???
      case s: ShExDocParser.ShapeAtomNonLiteralContext => ???
      case s: ShExDocParser.ShapeAtomDataTypeContext => ???
      case s: ShExDocParser.ShapeAtomGroupContext => ???
      case s: ShExDocParser.ShapeAtomValueSetContext => ???
      case s: ShExDocParser.ShapeAtomShapeExpressionContext => ???
      case s: ShExDocParser.ShapeAtomAnyContext => ???
    }
   }

   def isDefined[A](x:A): Boolean =
     if (x != null) true
     else false

   override def visitShapeLabel(ctx: ShExDocParser.ShapeLabelContext): Builder[ShapeLabel] = {
     if (isDefined(ctx.iri())) {
       for {
        iri <- visitIri(ctx.iri())
       } yield IRILabel(iri)
     } else {
       ??? // BNodeLabel(visitBlankNode(ctx.blankNode()))
     }
   }

   override def visitIri(ctx: ShExDocParser.IriContext): Builder[IRI] = {
     if (isDefined(ctx.IRIREF())) {
       ok(extractIRIfromIRIREF(ctx.IRIREF().getText))
     } else {
       val prefixedName = visitPrefixedName(ctx.prefixedName())
       resolve(prefixedName)
     }
   }

   type PrefixedName = (String,Option[String])

   def resolve(prefixedName: PrefixedName): Builder[IRI] = {
     val (p,local) = prefixedName
     val prefix = Prefix(p)
     getPrefixMap.flatMap(prefixMap =>
       prefixMap.get(prefix) match {
         case None => err(s"Prefix $p not found in current prefix map $prefixMap")
         case Some(iri) => local match {
           case None => ok(iri)
           case Some(name) => ok(iri + name)
         }
   })
  }


/*
     val prefix = Prefix(prefixedName._1)
     if (currentPrefixMap.isDefinedAt(prefix)) {
       Right(currentPrefixMap(prefix))
     } else {
       Left(s"prefix $prefix not found in current map $currentPrefixMap")
     } */

   override def visitPrefixedName(ctx: ShExDocParser.PrefixedNameContext): PrefixedName = {
     if (isDefined(ctx.PNAME_LN())) {
       (ctx.PNAME_NS().getText(),Some(ctx.PNAME_LN().getText()))
     } else {
       (ctx.PNAME_NS().getText(),None)
     }
   }

   override def visitBlankNode(ctx: ShExDocParser.BlankNodeContext): BNodeId = {
     ???
   }

   def getPrefixes(ds: List[Directive]): Map[Prefix,IRI] = {
     def comb(rest: Map[Prefix,IRI],x: Directive): Map[Prefix,IRI] = {
       x.fold(p => rest + p, _ => rest)
     }
     def zero: Map[Prefix,IRI] = Map()
     ds.foldLeft(zero)(comb)
   }

/*   def getBase(ds: List[Directive]): Option[IRI] = {
     def comb(rest: Option[IRI],x: Directive): Option[IRI] = {
       x.fold(_ => rest, iri => combineBase(rest,iri))
     }
     def zero: Option[IRI] = None
     ds.foldLeft(zero)(comb)
   } */

/*   def combineBase(rest: Option[IRI], iri: IRI): Option[IRI] = {
     rest match {
       case None => Some(iri)
       case Some(i) => Some(iri) // Combine if iri is a relative IRI?
     }
   } */

   override def visitDirective(
     ctx: ShExDocParser.DirectiveContext): Builder[Directive] ={
     if (ctx.baseDecl() != null) {
       for {
         iri <- visitBaseDecl(ctx.baseDecl())
       } yield Right(iri)
     } else {
       for {
        p <- visitPrefixDecl(ctx.prefixDecl())
      } yield Left(p)
     }
   }

   override def visitBaseDecl(ctx: ShExDocParser.BaseDeclContext): Builder[IRI] = {
     val baseIri = extractIRIfromIRIREF(ctx.IRIREF().getText)
     for {
       _ <- addBase(baseIri)
     } yield baseIri
   }

   override def visitPrefixDecl(ctx: ShExDocParser.PrefixDeclContext): Builder[(Prefix,IRI)] = {
     val prefix = Prefix(ctx.PNAME_NS().getText)
     val iri = extractIRIfromIRIREF(ctx.IRIREF().getText)
     for {
       _ <- addPrefix(prefix,iri)
     } yield (prefix,iri)
   }

   type Directive = Either[(Prefix,IRI), IRI]

   def extractIRIfromIRIREF(d: String): IRI = {
     val iriRef = "^<(.*)>$".r
     d match {
       case iriRef(i) => IRI(i)
     }
   }

 }
}
