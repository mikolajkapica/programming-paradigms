file://<WORKSPACE>/l04.scala
### java.lang.AssertionError: assertion failed

occurred in the presentation compiler.

action parameters:
offset: 489
uri: file://<WORKSPACE>/l04.scala
text:
```scala
sealed trait Tree3[+A] 
case class Node[A](element: A, left: Tree3[A], center:  Tree3[A], right: Tree3[A]) extends Tree3[A]
case object Empty extends Tree3[Nothing]

def mapTree3[A, B](f: A => B): Tree3[A] => Tree3[B] =
    (t: Tree3[A]) => t match {
        case Empty => Empty
        case Node(e, l, c, r) => Node(f(e), mapTree3(f)(l), mapTree3(f)(c), mapTree3(f)(r))
    }

type Data = String
type Name = String
type Letter = Char

case class File(name: Name, data: Data) extends Item
@@case class Folder(name: Name, items: List[Item]) extends Item

case class Disk(letter: Letter, items: List[Item])

```



#### Error stacktrace:

```
scala.runtime.Scala3RunTime$.assertFailed(Scala3RunTime.scala:11)
	dotty.tools.dotc.core.Annotations$LazyAnnotation.tree(Annotations.scala:136)
	dotty.tools.dotc.core.Annotations$Annotation$Child$.unapply(Annotations.scala:242)
	dotty.tools.dotc.typer.Namer.insertInto$1(Namer.scala:477)
	dotty.tools.dotc.typer.Namer.addChild(Namer.scala:488)
	dotty.tools.dotc.typer.Namer$Completer.register$1(Namer.scala:899)
	dotty.tools.dotc.typer.Namer$Completer.registerIfChild$$anonfun$1(Namer.scala:908)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	scala.collection.immutable.List.foreach(List.scala:333)
	dotty.tools.dotc.typer.Namer$Completer.registerIfChild(Namer.scala:908)
	dotty.tools.dotc.typer.Namer$Completer.complete(Namer.scala:811)
	dotty.tools.dotc.core.SymDenotations$SymDenotation.completeFrom(SymDenotations.scala:174)
	dotty.tools.dotc.core.Denotations$Denotation.completeInfo$1(Denotations.scala:187)
	dotty.tools.dotc.core.Denotations$Denotation.info(Denotations.scala:189)
	dotty.tools.dotc.core.Types$NamedType.info(Types.scala:2308)
	dotty.tools.dotc.core.Types$TermLambda.dotty$tools$dotc$core$Types$TermLambda$$_$compute$1(Types.scala:3791)
	dotty.tools.dotc.core.Types$TermLambda.foldArgs$2(Types.scala:3798)
	dotty.tools.dotc.core.Types$TermLambda.dotty$tools$dotc$core$Types$TermLambda$$_$compute$1(Types.scala:4409)
	dotty.tools.dotc.core.Types$TermLambda.dotty$tools$dotc$core$Types$TermLambda$$depStatus(Types.scala:3818)
	dotty.tools.dotc.core.Types$TermLambda.dependencyStatus(Types.scala:3832)
	dotty.tools.dotc.core.Types$TermLambda.isResultDependent(Types.scala:3854)
	dotty.tools.dotc.core.Types$TermLambda.isResultDependent$(Types.scala:3748)
	dotty.tools.dotc.core.Types$MethodType.isResultDependent(Types.scala:3892)
	dotty.tools.dotc.typer.TypeAssigner.assignType(TypeAssigner.scala:288)
	dotty.tools.dotc.typer.TypeAssigner.assignType$(TypeAssigner.scala:16)
	dotty.tools.dotc.typer.Typer.assignType(Typer.scala:115)
	dotty.tools.dotc.ast.tpd$.Apply(tpd.scala:49)
	dotty.tools.dotc.core.tasty.TreeUnpickler.dotty$tools$dotc$core$tasty$TreeUnpickler$TreeReader$$_$constructorApply$1(TreeUnpickler.scala:1257)
	dotty.tools.dotc.core.tasty.TreeUnpickler$TreeReader.readLengthTerm$1(TreeUnpickler.scala:1284)
	dotty.tools.dotc.core.tasty.TreeUnpickler$TreeReader.readTerm(TreeUnpickler.scala:1449)
	dotty.tools.dotc.core.tasty.TreeUnpickler.$anonfun$15$$anonfun$1(TreeUnpickler.scala:741)
	dotty.tools.dotc.core.tasty.TreeUnpickler$LazyReader.complete(TreeUnpickler.scala:1586)
	dotty.tools.dotc.core.tasty.TreeUnpickler.$anon$superArg$2$1$$anonfun$1(TreeUnpickler.scala:743)
	dotty.tools.dotc.core.Annotations$LazyAnnotation.tree(Annotations.scala:140)
	dotty.tools.dotc.core.Annotations$Annotation$Child$.unapply(Annotations.scala:242)
	dotty.tools.dotc.typer.Namer.insertInto$1(Namer.scala:477)
	dotty.tools.dotc.typer.Namer.addChild(Namer.scala:488)
	dotty.tools.dotc.typer.Namer$Completer.register$1(Namer.scala:899)
	dotty.tools.dotc.typer.Namer$Completer.registerIfChild$$anonfun$1(Namer.scala:908)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	scala.collection.immutable.List.foreach(List.scala:333)
	dotty.tools.dotc.typer.Namer$Completer.registerIfChild(Namer.scala:908)
	dotty.tools.dotc.typer.Namer$Completer.complete(Namer.scala:811)
	dotty.tools.dotc.core.SymDenotations$SymDenotation.completeFrom(SymDenotations.scala:174)
	dotty.tools.dotc.core.Denotations$Denotation.completeInfo$1(Denotations.scala:187)
	dotty.tools.dotc.core.Denotations$Denotation.info(Denotations.scala:189)
	dotty.tools.dotc.core.SymDenotations$SymDenotation.ensureCompleted(SymDenotations.scala:390)
	dotty.tools.dotc.typer.Typer.retrieveSym(Typer.scala:2869)
	dotty.tools.dotc.typer.Typer.typedNamed$1(Typer.scala:2894)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:2990)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3058)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3062)
	dotty.tools.dotc.typer.Typer.traverse$1(Typer.scala:3084)
	dotty.tools.dotc.typer.Typer.typedStats(Typer.scala:3130)
	dotty.tools.dotc.typer.Typer.typedPackageDef(Typer.scala:2692)
	dotty.tools.dotc.typer.Typer.typedUnnamed$1(Typer.scala:2961)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:2991)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3058)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3062)
	dotty.tools.dotc.typer.Typer.typedExpr(Typer.scala:3174)
	dotty.tools.dotc.typer.TyperPhase.typeCheck$$anonfun$1(TyperPhase.scala:44)
	dotty.tools.dotc.typer.TyperPhase.typeCheck$$anonfun$adapted$1(TyperPhase.scala:54)
	scala.Function0.apply$mcV$sp(Function0.scala:42)
	dotty.tools.dotc.core.Phases$Phase.monitor(Phases.scala:437)
	dotty.tools.dotc.typer.TyperPhase.typeCheck(TyperPhase.scala:54)
	dotty.tools.dotc.typer.TyperPhase.runOn$$anonfun$3(TyperPhase.scala:88)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	scala.collection.immutable.List.foreach(List.scala:333)
	dotty.tools.dotc.typer.TyperPhase.runOn(TyperPhase.scala:88)
	dotty.tools.dotc.Run.runPhases$1$$anonfun$1(Run.scala:247)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	scala.collection.ArrayOps$.foreach$extension(ArrayOps.scala:1321)
	dotty.tools.dotc.Run.runPhases$1(Run.scala:263)
	dotty.tools.dotc.Run.compileUnits$$anonfun$1(Run.scala:271)
	dotty.tools.dotc.Run.compileUnits$$anonfun$adapted$1(Run.scala:280)
	dotty.tools.dotc.util.Stats$.maybeMonitored(Stats.scala:67)
	dotty.tools.dotc.Run.compileUnits(Run.scala:280)
	dotty.tools.dotc.Run.compileSources(Run.scala:195)
	dotty.tools.dotc.interactive.InteractiveDriver.run(InteractiveDriver.scala:165)
	scala.meta.internal.pc.MetalsDriver.run(MetalsDriver.scala:45)
	scala.meta.internal.pc.PcCollector.<init>(PcCollector.scala:42)
	scala.meta.internal.pc.PcDocumentHighlightProvider.<init>(PcDocumentHighlightProvider.scala:16)
	scala.meta.internal.pc.ScalaPresentationCompiler.documentHighlight$$anonfun$1(ScalaPresentationCompiler.scala:155)
```
#### Short summary: 

java.lang.AssertionError: assertion failed