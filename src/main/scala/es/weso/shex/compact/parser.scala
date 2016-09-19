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
    maker.visit(parser.shExDoc()) match {
      case s: Schema => Right(s)
      case e => Left(s"Unknown type of value $e")
    }
  }

 class SchemaMaker extends ShExDocBaseVisitor[Any] {
   type Start = Option[ShapeExpr]
   type NotStartAction = Either[Start,Shape]

   override def visitShExDoc(ctx: ShExDocParser.ShExDocContext): Builder[Schema] = ???
/*     for {
       startDirectives <- ctx.directive().asScala.map(visitDirective(_)).toList.sequence
       notStartAction <-
         if (isDefined(ctx.notStartAction())) {
         visitNotStartAction(ctx.notStartAction())
         } else
         ???
    } yield Schema.empty.copy(
           prefixes = if (prefixes.isEmpty) None else Some(prefixes),
           base = base
         )
   } */

   override def visitNotStartAction(ctx: ShExDocParser.NotStartActionContext): Builder[NotStartAction] = {
     ???
/*     if (ctx.start() != null) {
       Left(visitStart(ctx.start()))
     } else {
       Right(visitShape(ctx.shape()))
     } */
   }

   override def visitStart(ctx: ShExDocParser.StartContext): Option[ShapeExpr] = {
     ???
   }

   override def visitShape(ctx: ShExDocParser.ShapeContext): Builder[ShapeLabel] =
    for {
     label <- visitShapeLabel(ctx.shapeLabel())
   } yield label

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

   def getBase(ds: List[Directive]): Option[IRI] = {
     def comb(rest: Option[IRI],x: Directive): Option[IRI] = {
       x.fold(_ => rest, iri => combineBase(rest,iri))
     }
     def zero: Option[IRI] = None
     ds.foldLeft(zero)(comb)
   }

   def combineBase(rest: Option[IRI], iri: IRI): Option[IRI] = {
     rest match {
       case None => Some(iri)
       case Some(i) => Some(iri) // Combine if iri is a relative IRI?
     }
   }

   override def visitDirective(ctx: ShExDocParser.DirectiveContext): Builder[Directive] ={
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
