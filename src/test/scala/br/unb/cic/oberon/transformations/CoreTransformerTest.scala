package br.unb.cic.oberon.transformations

import br.unb.cic.oberon.parser.ScalaParser
import br.unb.cic.oberon.interpreter.Interpreter
import org.scalatest.funsuite.AnyFunSuite
import br.unb.cic.oberon.ast._
import java.nio.file.{Files, Paths}

class CoreTransformerTest extends AnyFunSuite {

  ignore("Testing the loop_stmt01 expressions after conversion to While") {
    val path = Paths.get(getClass.getClassLoader.getResource("stmts/loop_stmt01.oberon").toURI)

    assert(path != null)

    val content = String.join("\n", Files.readAllLines(path))
    val module = ScalaParser.parse(content)
    val interpreter = new Interpreter()
    val coreVisitor = new CoreVisitor()

    val coreModule = coreVisitor.transformModule(module)

    assert(coreModule.name == "LoopStmt")
    assert (coreModule.stmt.isDefined)
    // assert that the main block contains a sequence of statements
    module.stmt.get match {
      case SequenceStmt(stmts) => assert(stmts.length == 2)
      case _ => fail("we are expecting two stmts in the main block")
    }

    // now we can assume that the main block contains a sequence of stmts
    val sequence = coreModule.stmt.get.asInstanceOf[SequenceStmt]
    val stmts = sequence.stmts

    assert(stmts.head == AssignmentStmt(VarAssignment("x"),IntValue(10)))
    assert(stmts(1) == WhileStmt(BoolValue(true), SequenceStmt(List(
      WriteStmt(VarExpression("x")),
      IfElseStmt(LTExpression(VarExpression("x"),IntValue(0)),
        ExitStmt(),
        None),
      AssignmentStmt(VarAssignment("x"),SubExpression(VarExpression("x"),IntValue(1)))
    ))))
  }

  ignore("Testing the loop_stmt02 evaluation after conversion to OberonCore") {
    val path = Paths.get(getClass.getClassLoader.getResource("stmts/loop_stmt02.oberon").toURI)

    assert(path != null)

    val content = String.join("\n", Files.readAllLines(path))
    val module = ScalaParser.parse(content)
    val interpreter = new Interpreter()
    val coreVisitor = new CoreVisitor()

    val coreModule = coreVisitor.transformModule(module)

    coreModule.accept(interpreter)
    assert(interpreter.env.lookup("x") == Some(IntValue(6)))
    assert(interpreter.env.lookup("factorial") == Some(IntValue(120)))
  }

  ignore("Testing the loop_stmt02 expressions after conversion to While") {
    val path = Paths.get(getClass.getClassLoader.getResource("stmts/loop_stmt02.oberon").toURI)

    assert(path != null)

    val content = String.join("\n", Files.readAllLines(path))
    val module = ScalaParser.parse(content)
    val interpreter = new Interpreter()
    val coreVisitor = new CoreVisitor()

    val coreModule = coreVisitor.transformModule(module)

    assert(coreModule.name == "LoopStmt")
    assert (coreModule.stmt.isDefined)

    // assert that the main block contains a sequence of statements
    module.stmt.get match {
      case SequenceStmt(stmts) => assert(stmts.length == 3)
      case _ => fail("we are expecting three stmts in the main block")
    }

    // now we can assume that the main block contains a sequence of stmts
    val sequence = coreModule.stmt.get.asInstanceOf[SequenceStmt]
    val stmts = sequence.stmts

    assert(stmts.head == AssignmentStmt(VarAssignment("x"),IntValue(2)))
    assert(stmts(1) == AssignmentStmt(VarAssignment("factorial"),IntValue(1)))
    assert(stmts(2) == WhileStmt(BoolValue(true), SequenceStmt(List(
      IfElseStmt(GTExpression(VarExpression("x"), IntValue(5)),
        ExitStmt(),
        None),
      AssignmentStmt(VarAssignment("factorial"), MultExpression(VarExpression("factorial"),VarExpression("x"))),
      AssignmentStmt(VarAssignment("x"), AddExpression(VarExpression("x"), IntValue(1)))
    ))))
  }

  ignore("Testing the loop_stmt03 evaluation after conversion to OberonCore") {
    val path = Paths.get(getClass.getClassLoader.getResource("stmts/loop_stmt03.oberon").toURI)

    assert(path != null)

    val content = String.join("\n", Files.readAllLines(path))
    val module = ScalaParser.parse(content)
    val interpreter = new Interpreter()
    val coreVisitor = new CoreVisitor()

    val coreModule = coreVisitor.transformModule(module)

    coreModule.accept(interpreter)
    assert(interpreter.env.lookup("x") == Some(IntValue(10)))
    assert(interpreter.env.lookup("i") == Some(IntValue(10)))
    assert(interpreter.env.lookup("y") == Some(IntValue(100)))
  }

  ignore("Testing the loop_stmt03 expressions after conversion to While") {
    val path = Paths.get(getClass.getClassLoader.getResource("stmts/loop_stmt03.oberon").toURI)

    assert(path != null)

    val content = String.join("\n", Files.readAllLines(path))
    val module = ScalaParser.parse(content)
    val interpreter = new Interpreter()
    val coreVisitor = new CoreVisitor()

    val coreModule = coreVisitor.transformModule(module)

    assert(coreModule.name == "LoopStmt")
    assert (coreModule.stmt.isDefined)
    // assert that the main block contains a sequence of statements
    module.stmt.get match {
      case SequenceStmt(stmts) => assert(stmts.length == 3)
      case _ => fail("we are expecting three stmts in the main block")
    }

    // now we can assume that the main block contains a sequence of stmts
    val sequence = coreModule.stmt.get.asInstanceOf[SequenceStmt]
    val stmts = sequence.stmts

    assert(stmts.head == AssignmentStmt(VarAssignment("x"),IntValue(0)))
    assert(stmts(1) == AssignmentStmt(VarAssignment("y"),IntValue(0)))
    assert(stmts(2) == WhileStmt(BoolValue(true), SequenceStmt(List(
      AssignmentStmt(VarAssignment("x"), AddExpression(VarExpression("x"),IntValue(1))),
      AssignmentStmt(VarAssignment("i"),IntValue(0)),
      WhileStmt(BoolValue(true), SequenceStmt(List(
        AssignmentStmt(VarAssignment("y"), AddExpression(VarExpression("y"),IntValue(1))),
        AssignmentStmt(VarAssignment("i"), AddExpression(VarExpression("i"),IntValue(1))),
        IfElseStmt(EQExpression(VarExpression("i"), IntValue(10)),
          ExitStmt(),
          None)
      ))),
      IfElseStmt(EQExpression(VarExpression("x"), IntValue(10)),
        ExitStmt(),
        None)
    ))))
  }

  /** ###### Loop Tests end here ###### */

  /** ###### RepeatUntil Tests begin here ###### */
  ignore("Testing the RepeatUntilStmt01 evaluation after conversion to OberonCore") {
    val path = Paths.get(getClass.getClassLoader.getResource("stmts/RepeatUntilStmt01.oberon").toURI)

    assert(path != null)

    val content = String.join("\n", Files.readAllLines(path))
    val module = ScalaParser.parse(content)
    val interpreter = new Interpreter()
    val coreVisitor = new CoreVisitor()

    val coreModule = coreVisitor.transformModule(module)

    coreModule.accept(interpreter)

    assert(module.name == "RepeatUntilModule");
    assert(interpreter.env.lookup("x").contains(IntValue(11)));
    assert(interpreter.env.lookup("sum").contains(IntValue(55)));
  }

  ignore("Testing the RepeatUntilStmt01 expressions after conversion to While") {
    val path = Paths.get(getClass.getClassLoader.getResource("stmts/RepeatUntilStmt01.oberon").toURI)

    assert(path != null)

    val content = String.join("\n", Files.readAllLines(path))
    val module = ScalaParser.parse(content)
    val coreVisitor = new CoreVisitor()

    val coreModule = coreVisitor.transformModule(module)

    assert(coreModule.name == "RepeatUntilModule")
    assert (coreModule.stmt.isDefined)
    // assert that the main block contains a sequence of statements
    module.stmt.get match {
      case SequenceStmt(stmts) => assert(stmts.length == 4)
      case _ => fail("we are expecting three stmts in the main block")
    }
    // now we can assume that the main block contains a sequence of stmts
    val sequence = coreModule.stmt.get.asInstanceOf[SequenceStmt]
    val stmts = sequence.stmts

    assert(stmts.head == AssignmentStmt(VarAssignment("x"),IntValue(0)))
    assert(stmts(1) == AssignmentStmt(VarAssignment("lim"),IntValue(10)))
    assert(stmts(2) == WhileStmt(BoolValue(true),SequenceStmt(List(
      IfElseStmt(EQExpression(VarExpression("x"), IntValue(0)),
        AssignmentStmt(VarAssignment("sum"),IntValue(0)),
        Some(AssignmentStmt(VarAssignment("sum"),AddExpression(VarExpression("sum"),VarExpression("x"))))
      ),
      AssignmentStmt(VarAssignment("x"), AddExpression(VarExpression("x"), IntValue(1))),
      IfElseStmt(GTExpression(VarExpression("x"),VarExpression("lim")),
        ExitStmt(),
        None)
    ))))
    assert(stmts(3) == WriteStmt(VarExpression("sum")))
  }

  // TODO
  ignore("Testing the RepeatUntilStmt06 evaluation after conversion to OberonCore") {
    val path = Paths.get(getClass.getClassLoader.getResource("stmts/RepeatUntilStmt06.oberon").toURI)

    assert(path != null)

    val content = String.join("\n", Files.readAllLines(path))
    val module = ScalaParser.parse(content)
    val interpreter = new Interpreter()
    val coreVisitor = new CoreVisitor()

    val coreModule = coreVisitor.transformModule(module)

    coreModule.accept(interpreter)

    assert(module.name == "RepeatUntilModule");
    assert(interpreter.env.lookup("x").contains(IntValue(11)))
    assert(interpreter.env.lookup("y").contains(IntValue(40)))
  }

  ignore("Testing the RepeatUntilStmt06 expressions after conversion to While") {
    val path = Paths.get(getClass.getClassLoader.getResource("stmts/RepeatUntilStmt06.oberon").toURI)

    assert(path != null)

    val content = String.join("\n", Files.readAllLines(path))
    val module = ScalaParser.parse(content)
    val coreVisitor = new CoreVisitor()

    val coreModule = coreVisitor.transformModule(module)

    assert(coreModule.name == "RepeatUntilModule")
    assert (coreModule.stmt.isDefined)
    // assert that the main block contains a sequence of statements
    module.stmt.get match {
      case SequenceStmt(stmts) => assert(stmts.length == 4)
      case _ => fail("we are expecting three stmts in the main block")
    }
    // now we can assume that the main block contains a sequence of stmts
    val sequence = coreModule.stmt.get.asInstanceOf[SequenceStmt]
    val stmts = sequence.stmts

    assert(stmts.head == AssignmentStmt(VarAssignment("x"),IntValue(0)))
    assert(stmts(1) == AssignmentStmt(VarAssignment("y"),IntValue(0)))
    assert(stmts(2) == WhileStmt(BoolValue(true), SequenceStmt(List(
      IfElseStmt(GTExpression(VarExpression("x"),IntValue(5)),
        AssignmentStmt(VarAssignment("y"), AddExpression(VarExpression("y"), VarExpression("x"))),
        None
      ),
      AssignmentStmt(VarAssignment("x"), AddExpression(VarExpression("x"), IntValue(1))),
      IfElseStmt(GTExpression(VarExpression("x"), IntValue(10)),
        ExitStmt(),
        None)
    ))))
    assert(stmts(3) == WriteStmt(VarExpression("y")))
  }

  /**RepeatUntil Test 07*/
  ignore("Testing the RepeatUntilStmt07 evaluation after conversion to OberonCore") {
    val path = Paths.get(getClass.getClassLoader.getResource("stmts/RepeatUntilStmt07.oberon").toURI)

    assert(path != null)

    val content = String.join("\n", Files.readAllLines(path))
    val module = ScalaParser.parse(content)
    val interpreter = new Interpreter()
    val coreVisitor = new CoreVisitor()

    val coreModule = coreVisitor.transformModule(module)

    coreModule.accept(interpreter)

    assert(module.name == "RepeatUntilModule");
    assert(interpreter.env.lookup("x").contains(IntValue(20)));
    assert(interpreter.env.lookup("y").contains(IntValue(20)));
  }

  ignore("Testing the RepeatUntilStmt07 expressions after conversion to While") {
    val path = Paths.get(getClass.getClassLoader.getResource("stmts/RepeatUntilStmt07.oberon").toURI)

    assert(path != null)

    val content = String.join("\n", Files.readAllLines(path))
    val module = ScalaParser.parse(content)
    val coreVisitor = new CoreVisitor()

    val coreModule = coreVisitor.transformModule(module)

    assert(coreModule.name == "RepeatUntilModule")
    assert (coreModule.stmt.isDefined)
    // assert that the main block contains a sequence of statements
    module.stmt.get match {
      case SequenceStmt(stmts) => assert(stmts.length == 4)
      case _ => fail("we are expecting three stmts in the main block")
    }
    // now we can assume that the main block contains a sequence of stmts
    val sequence = coreModule.stmt.get.asInstanceOf[SequenceStmt]
    val stmts = sequence.stmts

    assert(stmts.head == AssignmentStmt(VarAssignment("x"),IntValue(0)))
    assert(stmts(1) == AssignmentStmt(VarAssignment("y"),IntValue(20)))
    assert(stmts(2) == WhileStmt(BoolValue(true), SequenceStmt(List(
      AssignmentStmt(VarAssignment("x"), AddExpression(VarExpression("x"), IntValue(1))),
      IfElseStmt(EQExpression(VarExpression("x"), VarExpression("y")),
        ExitStmt(),
        None)
    ))))
    assert(stmts(3) == WriteStmt(VarExpression("x")))
  }

  ignore("Testing the repeatuntil02 evaluation after conversion to OberonCore") {
    val path = Paths.get(getClass.getClassLoader.getResource("stmts/repeatuntil02.oberon").toURI)

    assert(path != null)

    val content = String.join("\n", Files.readAllLines(path))
    val module = ScalaParser.parse(content)
    val interpreter = new Interpreter()
    val coreVisitor = new CoreVisitor()

    val coreModule = coreVisitor.transformModule(module)

    coreModule.accept(interpreter)

    assert(module.name == "RepeatUntilModule");

    println(interpreter.env.lookup("x"))
    println(interpreter.env.lookup("y"))

    assert(interpreter.env.lookup("x").contains(IntValue(10)));
    assert(interpreter.env.lookup("y").contains(IntValue(19)));
  }

  ignore("Testing the repeatuntil02 expressions after conversion to While") {
    val path = Paths.get(getClass.getClassLoader.getResource("stmts/repeatuntil02.oberon").toURI)

    assert(path != null)

    val content = String.join("\n", Files.readAllLines(path))
    val module = ScalaParser.parse(content)
    val coreVisitor = new CoreVisitor()

    val coreModule = coreVisitor.transformModule(module)

    assert(coreModule.name == "RepeatUntilModule")
    assert (coreModule.stmt.isDefined)
    // assert that the main block contains a sequence of statements
    module.stmt.get match {
      case SequenceStmt(stmts) => assert(stmts.length == 5)
      case _ => fail("we are expecting three stmts in the main block")
    }
    // now we can assume that the main block contains a sequence of stmts
    val sequence = coreModule.stmt.get.asInstanceOf[SequenceStmt]
    val stmts = sequence.stmts

    assert(stmts.head == AssignmentStmt(VarAssignment("x"),IntValue(0)))
    assert(stmts(1) == AssignmentStmt(VarAssignment("y"),IntValue(0)))

    assert(stmts(2) == WhileStmt(BoolValue(true), SequenceStmt(List(
      AssignmentStmt(VarAssignment("x"),AddExpression(VarExpression("x"),IntValue(1))),
      WhileStmt(BoolValue(true), SequenceStmt(List(
        AssignmentStmt(VarAssignment("y"),AddExpression(VarExpression("y"),IntValue(1))),
        IfElseStmt(GTEExpression(VarExpression("y"),IntValue(10)),
          ExitStmt(),
          None
        )
      ))),
      IfElseStmt(GTEExpression(VarExpression("x"),IntValue(10)),
        ExitStmt(),
        None)
    ))))
    assert(stmts(3) == WriteStmt(VarExpression("x")))
    assert(stmts(4) == WriteStmt(VarExpression("y")))
  }

  ignore("Testing the repeatuntil04 evaluation after conversion to OberonCore") {
    val path = Paths.get(getClass.getClassLoader.getResource("stmts/repeatuntil04.oberon").toURI)

    assert(path != null)

    val content = String.join("\n", Files.readAllLines(path))
    val module = ScalaParser.parse(content)
    val interpreter = new Interpreter()
    val coreVisitor = new CoreVisitor()

    val coreModule = coreVisitor.transformModule(module)

    coreModule.accept(interpreter)

    assert(module.name == "RepeatUntilModule");

    println(interpreter.env.lookup("x"))
    println(interpreter.env.lookup("y"))

    assert(interpreter.env.lookup("x").contains(IntValue(2)));
    assert(interpreter.env.lookup("y").contains(IntValue(2)));
  }

  ignore("Testing the repeatuntil04 expressions after conversion to While") {
    val path = Paths.get(getClass.getClassLoader.getResource("stmts/repeatuntil04.oberon").toURI)

    assert(path != null)

    val content = String.join("\n", Files.readAllLines(path))
    val module = ScalaParser.parse(content)
    val coreVisitor = new CoreVisitor()

    val coreModule = coreVisitor.transformModule(module)

    assert(coreModule.name == "RepeatUntilModule")
    assert (coreModule.stmt.isDefined)

    val sequence = coreModule.stmt.get.asInstanceOf[SequenceStmt]
    val stmts = sequence.stmts
    // assert that the main block contains a sequence of statements
    module.stmt.get match {
      case SequenceStmt(stmts) => assert(stmts.length == 3)
      case _ => fail("we are expecting three stmts in the main block")
    }
    // now we can assume that the main block contains a sequence of stmts
    assert(stmts.head == AssignmentStmt(VarAssignment("x"), IntValue(2)))
    assert(stmts(1) == AssignmentStmt(VarAssignment("y"), IntValue(2)))
    assert(stmts(2) == WriteStmt(FunctionCallExpression("power",List(VarExpression("x"), VarExpression("y")))))
    assert(coreModule.procedures.head.stmt == SequenceStmt(List(AssignmentStmt(VarAssignment("r"),VarExpression("b")), IfElseStmt(OrExpression(LTExpression(VarExpression("b"),IntValue(0)),LTExpression(VarExpression("e"),IntValue(0))),ReturnStmt(IntValue(0)),None), IfElseStmt(EQExpression(VarExpression("e"),IntValue(0)),ReturnStmt(IntValue(1)),None), WhileStmt(BoolValue(true),SequenceStmt(List(AssignmentStmt(VarAssignment("r"),MultExpression(VarExpression("r"),VarExpression("b"))), AssignmentStmt(VarAssignment("e"),SubExpression(VarExpression("e"),IntValue(1))), IfElseStmt(LTEExpression(VarExpression("e"),IntValue(1)),ExitStmt(),None)))), ReturnStmt(VarExpression("r")))))
  }
  /** ###### RepeatUntil Tests end here ###### */

  /** ###### For Tests begin here ###### */
  ignore("Testing the interpreter_stmt01 evaluation after conversion to OberonCore") {
    val path = Paths.get(getClass.getClassLoader.getResource("stmts/interpreter_stmt01.oberon").toURI)

    assert(path != null)

    val content = String.join("\n", Files.readAllLines(path))
    val module = ScalaParser.parse(content)
    val interpreter = new Interpreter()
    val coreVisitor = new CoreVisitor()

    val coreModule = coreVisitor.transformModule(module)

    coreModule.accept(interpreter)

    assert(module.name == "SimpleModule")

    assert(interpreter.env.lookup("x") == Some(IntValue(5))) // FOR TO x
    assert(interpreter.env.lookup("y") == Some(IntValue(6))) // y = x + 1 (after last FOR)
    assert(interpreter.env.lookup("z") == Some(IntValue(15))) // z = result
  }

  ignore("Testing the interpreter_stmt01 expressions after conversion to While") {
    val path = Paths.get(getClass.getClassLoader.getResource("stmts/interpreter_stmt01.oberon").toURI)

    assert(path != null)

    val content = String.join("\n", Files.readAllLines(path))
    val module = ScalaParser.parse(content)
    val coreVisitor = new CoreVisitor()

    val coreModule = coreVisitor.transformModule(module)

    assert(coreModule.name == "SimpleModule")
    assert (coreModule.stmt.isDefined)
    // assert that the main block contains a sequence of statements
    module.stmt.get match {
      case SequenceStmt(stmts) => assert(stmts.length == 4)
      case _ => fail("we are expecting four stmts in the main block")
    }
    // now we can assume that the main block contains a sequence of stmts
    val sequence = coreModule.stmt.get.asInstanceOf[SequenceStmt]
    val stmts = sequence.stmts

    assert(stmts.head == AssignmentStmt(VarAssignment("x"), IntValue(5)))
    assert(stmts(1) == AssignmentStmt(VarAssignment("z"), IntValue(0)))
    assert(stmts(2) == AssignmentStmt(VarAssignment("y"), IntValue(0)))
    assert(stmts(3) == WhileStmt(LTEExpression(VarExpression("y"), VarExpression("x")), SequenceStmt(List(
      AssignmentStmt(VarAssignment("z"), AddExpression(VarExpression("z"), VarExpression("y"))),
      AssignmentStmt(VarAssignment("y"), AddExpression(VarExpression("y"), IntValue(1)))
    ))))
    assert(stmts(4) == WriteStmt(VarExpression("z")))
  }


  ignore("Testing the stmtForCore01 evaluation after conversion to OberonCore") {
    val path = Paths.get(getClass.getClassLoader.getResource("stmts/stmtForCore01.oberon").toURI)

    assert(path != null)

    val content = String.join("\n", Files.readAllLines(path))
    val module = ScalaParser.parse(content)
    val interpreter = new Interpreter()
    val coreVisitor = new CoreVisitor()

    val coreModule = coreVisitor.transformModule(module)

    coreModule.accept(interpreter)

    assert(module.name == "SimpleModule")

    assert(interpreter.env.lookup("x") == Some(IntValue(10))) // FOR TO x
    assert(interpreter.env.lookup("k") == Some(IntValue(18))) // k = result
  }

  ignore("Testing the stmtForCore01 expressions after conversion to While") {
    val path = Paths.get(getClass.getClassLoader.getResource("stmts/stmtForCore01.oberon").toURI)

    assert(path != null)

    val content = String.join("\n", Files.readAllLines(path))
    val module = ScalaParser.parse(content)
    val coreVisitor = new CoreVisitor()

    val coreModule = coreVisitor.transformModule(module)

    assert(coreModule.name == "SimpleModule")
    assert (coreModule.stmt.isDefined)
    // assert that the main block contains a sequence of statements
    module.stmt.get match {
      case SequenceStmt(stmts) => assert(stmts.length == 2)
      case _ => fail("we are expecting two stmts in the main block")
    }
    // now we can assume that the main block contains a sequence of stmts
    val sequence = coreModule.stmt.get.asInstanceOf[SequenceStmt]
    val stmts = sequence.stmts

    assert(stmts.head == AssignmentStmt(VarAssignment("x"), IntValue(0)))
    assert(stmts(1) == WhileStmt(LTExpression(VarExpression("x"), VarExpression(("y"))), SequenceStmt(List(
      WriteStmt(VarExpression("x")),
      AssignmentStmt(VarAssignment("z"), IntValue(0)),
      WhileStmt(LTExpression(VarExpression("z"), VarExpression("y")), SequenceStmt(List(
        AssignmentStmt(VarAssignment("k"), AddExpression(VarExpression("z"), VarExpression("x"))),
        AssignmentStmt(VarAssignment("z"), AddExpression(VarExpression("z"), IntValue(1))),
        WriteStmt(VarExpression("k"))
      ))),
      AssignmentStmt(VarAssignment("x"), AddExpression(VarExpression("x"), IntValue(1)))
    ))))

  }
  /** ###### For Tests end here ###### */


  /** ###### IfElseIf Tests begin here ###### */
  ignore("Testing the IfElseIfStmt01 evaluation after conversion to OberonCore") {
    val path = Paths.get(getClass.getClassLoader.getResource("stmts/IfElseIfStmt01.oberon").toURI)

    assert(path != null)

    val content = String.join("\n", Files.readAllLines(path))
    val module = ScalaParser.parse(content)
    val interpreter = new Interpreter()
    val coreVisitor = new CoreVisitor()

    val coreModule = coreVisitor.transformModule(module)

    coreModule.accept(interpreter)

    assert(module.name == "SimpleModule")

    assert(interpreter.env.lookup("x") == Some(IntValue(1)));
    assert(interpreter.env.lookup("y") == Some(IntValue(1)));
  }

  ignore("Testing the IfElseIfStmt01 expressions after conversion to IfElse") {
    val path = Paths.get(getClass.getClassLoader.getResource("stmts/IfElseIfStmt01.oberon").toURI)

    assert(path != null)

    val content = String.join("\n", Files.readAllLines(path))
    val module = ScalaParser.parse(content)
    val coreVisitor = new CoreVisitor()

    val coreModule = coreVisitor.transformModule(module)

    assert(coreModule.name == "SimpleModule")
    assert (coreModule.stmt.isDefined)
    // assert that the main block contains a sequence of statements
    module.stmt.get match {
      case SequenceStmt(stmts) => assert(stmts.length == 3)
      case _ => fail("we are expecting three stmts in the main block")
    }
    // now we can assume that the main block contains a sequence of stmts
    val sequence = coreModule.stmt.get.asInstanceOf[SequenceStmt]
    val stmts = sequence.stmts

    assert(stmts.head == AssignmentStmt(VarAssignment("x"), IntValue(1)))
    assert(stmts(1) == IfElseStmt(LTExpression(VarExpression("x"), IntValue(10)),
      AssignmentStmt(VarAssignment("y"), IntValue(1)),
      Some(IfElseStmt(GTExpression(VarExpression("x"), IntValue(10)),
        AssignmentStmt(VarAssignment("y"), IntValue(2)),
        Some(AssignmentStmt(VarAssignment("y"), IntValue(3)))))
    ))
    assert((stmts(2) == WriteStmt(VarExpression("y"))))
  }


  ignore("Testing the IfElseIfStmt03 evaluation after conversion to OberonCore") {
    val path = Paths.get(getClass.getClassLoader.getResource("stmts/IfElseIfStmt03.oberon").toURI)

    assert(path != null)

    val content = String.join("\n", Files.readAllLines(path))
    val module = ScalaParser.parse(content)
    val interpreter = new Interpreter()
    val coreVisitor = new CoreVisitor()

    val coreModule = coreVisitor.transformModule(module)

    coreModule.accept(interpreter)

    assert(module.name == "SimpleModule")

    assert(interpreter.env.lookup("x") == Some(IntValue(10)));
    assert(interpreter.env.lookup("y") == Some(IntValue(3)));
  }

  ignore("Testing the IfElseIfStmt03 expressions after conversion to IfElse") {
    val path = Paths.get(getClass.getClassLoader.getResource("stmts/IfElseIfStmt03.oberon").toURI)

    assert(path != null)

    val content = String.join("\n", Files.readAllLines(path))
    val module = ScalaParser.parse(content)
    val coreVisitor = new CoreVisitor()

    val coreModule = coreVisitor.transformModule(module)

    assert(coreModule.name == "SimpleModule")
    assert (coreModule.stmt.isDefined)
    // assert that the main block contains a sequence of statements
    module.stmt.get match {
      case SequenceStmt(stmts) => assert(stmts.length == 3)
      case _ => fail("we are expecting three stmts in the main block")
    }
    // now we can assume that the main block contains a sequence of stmts
    val sequence = coreModule.stmt.get.asInstanceOf[SequenceStmt]
    val stmts = sequence.stmts

    assert(stmts.head == AssignmentStmt(VarAssignment("x"), IntValue(10)))
    assert(stmts(1) == IfElseStmt(LTExpression(VarExpression("x"), IntValue(10)),
      AssignmentStmt(VarAssignment("y"), IntValue(1)),
      Some(IfElseStmt(GTExpression(VarExpression("x"), IntValue(10)),
        AssignmentStmt(VarAssignment("y"), IntValue(2)),
        Some(AssignmentStmt(VarAssignment("y"), IntValue(3)))))
    ))
    assert((stmts(2) == WriteStmt(VarExpression("y"))))
  }

  ignore("Testing the IfElseIfStmt05 evaluation after conversion to OberonCore") {
    val path = Paths.get(getClass.getClassLoader.getResource("stmts/IfElseIfStmt05.oberon").toURI)

    assert(path != null)

    val content = String.join("\n", Files.readAllLines(path))
    val module = ScalaParser.parse(content)
    val interpreter = new Interpreter()
    val coreVisitor = new CoreVisitor()

    val coreModule = coreVisitor.transformModule(module)

    coreModule.accept(interpreter)

    assert(module.name == "SimpleModule")

    assert(interpreter.env.lookup("x") == Some(IntValue(55)));
    assert(interpreter.env.lookup("y") == Some(IntValue(5)));
  }

  ignore("Testing the IfElseIfStmt05 expressions after conversion to IfElse") {
    val path = Paths.get(getClass.getClassLoader.getResource("stmts/IfElseIfStmt05.oberon").toURI)

    assert(path != null)

    val content = String.join("\n", Files.readAllLines(path))
    val module = ScalaParser.parse(content)
    val coreVisitor = new CoreVisitor()

    val coreModule = coreVisitor.transformModule(module)

    assert(coreModule.name == "SimpleModule")
    assert (coreModule.stmt.isDefined)
    // assert that the main block contains a sequence of statements
    module.stmt.get match {
      case SequenceStmt(stmts) => assert(stmts.length == 3)
      case _ => fail("we are expecting three stmts in the main block")
    }
    // now we can assume that the main block contains a sequence of stmts
    val sequence = coreModule.stmt.get.asInstanceOf[SequenceStmt]
    val stmts = sequence.stmts

    assert(stmts.head == AssignmentStmt(VarAssignment("x"), IntValue(55)))
    assert(stmts(1) == IfElseStmt(LTExpression(VarExpression("x"), IntValue(13)),
      AssignmentStmt(VarAssignment("y"), IntValue(1)),
      Some(IfElseStmt(LTExpression(VarExpression("x"), IntValue(21)),
        AssignmentStmt(VarAssignment("y"), IntValue(2)),
        Some(IfElseStmt(LTExpression(VarExpression("x"), IntValue(35)),
          AssignmentStmt(VarAssignment("y"), IntValue(3)),
          Some(IfElseStmt(LTExpression(VarExpression("x"), IntValue(50)),
            AssignmentStmt(VarAssignment("y"), IntValue(4)),
            Some(AssignmentStmt(VarAssignment("y"), IntValue(5)))))))))
    ))
    assert((stmts(2) == WriteStmt(VarExpression("y"))))
  }

  ignore("Testing the IfElseIfStmt08 evaluation after conversion to OberonCore") {
    val path = Paths.get(getClass.getClassLoader.getResource("stmts/IfElseIfStmt08.oberon").toURI)

    assert(path != null)

    val content = String.join("\n", Files.readAllLines(path))
    val module = ScalaParser.parse(content)
    val interpreter = new Interpreter()
    val coreVisitor = new CoreVisitor()

    val coreModule = coreVisitor.transformModule(module)

    coreModule.accept(interpreter)

    assert(module.name == "SimpleModule")

    assert(interpreter.env.lookup("y") == Some(IntValue(3)));
    assert(interpreter.env.lookup("x") == Some(IntValue(0)));
  }

  ignore("Testing the IfElseIfStmt08 expressions after conversion to IfElse") {
    val path = Paths.get(getClass.getClassLoader.getResource("stmts/IfElseIfStmt08.oberon").toURI)

    assert(path != null)

    val content = String.join("\n", Files.readAllLines(path))
    val module = ScalaParser.parse(content)
    val coreVisitor = new CoreVisitor()

    val coreModule = coreVisitor.transformModule(module)

    assert(coreModule.name == "SimpleModule")
    assert (coreModule.stmt.isDefined)
    // assert that the main block contains a sequence of statements
    module.stmt.get match {
      case SequenceStmt(stmts) => assert(stmts.length == 3)
      case _ => fail("we are expecting three stmts in the main block")
    }
    // now we can assume that the main block contains a sequence of stmts
    val sequence = coreModule.stmt.get.asInstanceOf[SequenceStmt]
    val stmts = sequence.stmts

    assert(stmts.head == AssignmentStmt(VarAssignment("x"), IntValue(0)))
    assert(stmts(1) == IfElseStmt(LTExpression(VarExpression("x"), IntValue(0)),
      AssignmentStmt(VarAssignment("y"), IntValue(1)),
      Some(IfElseStmt(GTExpression(VarExpression("x"), IntValue(0)),
        AssignmentStmt(VarAssignment("y"), IntValue(2)),
        Some(AssignmentStmt(VarAssignment("y"), IntValue(3)))))
    ))
    assert((stmts(2) == WriteStmt(VarExpression("y"))))
  }
  /** ###### IfElseIf Tests begin here ###### */


  /** ###### Case Tests begin here ###### */
  ignore("Testing the StmtCaseCore01 evaluation after conversion to OberonCore") {
    val path = Paths.get(getClass.getClassLoader.getResource("stmts/StmtCaseCore01.oberon").toURI)

    assert(path != null)

    val content = String.join("\n", Files.readAllLines(path))
    val module = ScalaParser.parse(content)
    val interpreter = new Interpreter()
    val coreVisitor = new CoreVisitor()

    val coreModule = coreVisitor.transformModule(module)

    coreModule.accept(interpreter)
    assert(module.name == "SimpleModule")

    assert(interpreter.env.lookup("xs") == Some(IntValue(0)));
  }

  ignore("Testing the StmtCaseCore01 expressions after conversion to IfElse") {
    val path = Paths.get(getClass.getClassLoader.getResource("stmts/StmtCaseCore01.oberon").toURI)

    assert(path != null)

    val content = String.join("\n", Files.readAllLines(path))
    val module = ScalaParser.parse(content)
    val coreVisitor = new CoreVisitor()

    val coreModule = coreVisitor.transformModule(module)

    assert(coreModule.name == "SimpleModule")
    assert (coreModule.stmt.isDefined)
    // assert that the main block contains a sequence of statements
    module.stmt.get match {
      case SequenceStmt(stmts) => assert(stmts.length == 3)
      case _ => fail("we are expecting three stmts in the main block")
    }
    // now we can assume that the main block contains a sequence of stmts
    val sequence = coreModule.stmt.get.asInstanceOf[SequenceStmt]
    val stmts = sequence.stmts

    assert(stmts.head == AssignmentStmt(VarAssignment("xs"), IntValue(0)))
    assert(stmts(1) == IfElseStmt(EQExpression(VarExpression("xs"), IntValue(1)),
      AssignmentStmt(VarAssignment("xs"), IntValue(5)),
      Some(IfElseStmt(EQExpression(VarExpression("xs"), IntValue(2)),
        AssignmentStmt(VarAssignment("xs"), IntValue(10)),
        Some(IfElseStmt(EQExpression(VarExpression("xs"), IntValue(3)),
          AssignmentStmt(VarAssignment("xs"), IntValue(20)),
          Some(IfElseStmt(EQExpression(VarExpression("xs"), IntValue(4)),
            AssignmentStmt(VarAssignment("xs"), IntValue(40)),
            Some(AssignmentStmt(VarAssignment("xs"), IntValue(0)))))))))
    ))
    assert((stmts(2) == WriteStmt(VarExpression("xs"))))
  }

  ignore("Testing the StmtCaseCore02 evaluation after conversion to OberonCore") {
    val path = Paths.get(getClass.getClassLoader.getResource("stmts/StmtCaseCore02.oberon").toURI)

    assert(path != null)

    val content = String.join("\n", Files.readAllLines(path))
    val module = ScalaParser.parse(content)
    val interpreter = new Interpreter()
    val coreVisitor = new CoreVisitor()

    val coreModule = coreVisitor.transformModule(module)

    coreModule.accept(interpreter)
    assert(module.name == "SimpleModule")

    assert(interpreter.env.lookup("xs") == Some(IntValue(10)));
  }

  ignore("Testing the StmtCaseCore02 expressions after conversion to IfElse") {
    val path = Paths.get(getClass.getClassLoader.getResource("stmts/StmtCaseCore02.oberon").toURI)

    assert(path != null)

    val content = String.join("\n", Files.readAllLines(path))
    val module = ScalaParser.parse(content)
    val coreVisitor = new CoreVisitor()

    val coreModule = coreVisitor.transformModule(module)

    assert(coreModule.name == "SimpleModule")
    assert (coreModule.stmt.isDefined)
    // assert that the main block contains a sequence of statements
    module.stmt.get match {
      case SequenceStmt(stmts) => assert(stmts.length == 3)
      case _ => fail("we are expecting three stmts in the main block")
    }
    // now we can assume that the main block contains a sequence of stmts
    val sequence = coreModule.stmt.get.asInstanceOf[SequenceStmt]
    val stmts = sequence.stmts

    assert(stmts.head == AssignmentStmt(VarAssignment("xs"), IntValue(2)))
    assert(stmts(1) == IfElseStmt(EQExpression(VarExpression("xs"), IntValue(1)),
      AssignmentStmt(VarAssignment("xs"), IntValue(5)),
      Some(IfElseStmt(EQExpression(VarExpression("xs"), IntValue(2)),
        AssignmentStmt(VarAssignment("xs"), IntValue(10)),
        Some(IfElseStmt(EQExpression(VarExpression("xs"), IntValue(3)),
          AssignmentStmt(VarAssignment("xs"), IntValue(20)),
          Some(IfElseStmt(EQExpression(VarExpression("xs"), IntValue(4)),
            AssignmentStmt(VarAssignment("xs"), IntValue(40)),
            Some(AssignmentStmt(VarAssignment("xs"), IntValue(0)))))))))
    ))
    assert((stmts(2) == WriteStmt(VarExpression("xs"))))
  }


  ignore("Testing the StmtCaseCore03 evaluation after conversion to OberoCore") {
    val path = Paths.get(getClass.getClassLoader.getResource("stmts/StmtCaseCore03.oberon").toURI)

    assert(path != null)

    val content = String.join("\n", Files.readAllLines(path))
    val module = ScalaParser.parse(content)
    val interpreter = new Interpreter()
    val coreVisitor = new CoreVisitor()

    val coreModule = coreVisitor.transformModule(module)

    coreModule.accept(interpreter)
    assert(module.name == "SimpleRangeCaseModule")

    assert(interpreter.env.lookup("xs") == Some(IntValue(5)));
  }

  ignore("Testing the StmtCaseCore03 expressions after conversion to IfElse") {
    val path = Paths.get(getClass.getClassLoader.getResource("stmts/StmtCaseCore03.oberon").toURI)

    assert(path != null)

    val content = String.join("\n", Files.readAllLines(path))
    val module = ScalaParser.parse(content)
    val coreVisitor = new CoreVisitor()

    val coreModule = coreVisitor.transformModule(module)

    assert(coreModule.name == "SimpleRangeCaseModule")
    assert (coreModule.stmt.isDefined)
    // assert that the main block contains a sequence of statements
    module.stmt.get match {
      case SequenceStmt(stmts) => assert(stmts.length == 5)
      case _ => fail("we are expecting five stmts in the main block")
    }
    // now we can assume that the main block contains a sequence of stmts
    val sequence = coreModule.stmt.get.asInstanceOf[SequenceStmt]
    val stmts = sequence.stmts

    assert(stmts.head == AssignmentStmt(VarAssignment("xs"), IntValue(1)))
    assert(stmts(1) == AssignmentStmt(VarAssignment("min"), IntValue(10)))
    assert(stmts(2) == AssignmentStmt(VarAssignment("max"), IntValue(20)))
    assert(stmts(3) == IfElseStmt(EQExpression(VarExpression("xs"), IntValue(1)),
      AssignmentStmt(VarAssignment("xs"), IntValue(5)),
      Some(IfElseStmt(EQExpression(VarExpression("xs"), IntValue(2)),
        AssignmentStmt(VarAssignment("xs"), IntValue(10)),
        Some(IfElseStmt(AndExpression(LTEExpression(VarExpression("min"), VarExpression("xs")), LTEExpression(VarExpression("xs"), VarExpression("max"))),
          AssignmentStmt(VarAssignment("xs"), IntValue(20)),
          Some(AssignmentStmt(VarAssignment("xs"), IntValue(0)))))))
    ))
    assert((stmts(4) == WriteStmt(VarExpression("xs"))))
  }

  ignore("Testing the StmtCaseCore04 evaluation after conversion to OberonCore") {
    val path = Paths.get(getClass.getClassLoader.getResource("stmts/StmtCaseCore04.oberon").toURI)

    assert(path != null)

    val content = String.join("\n", Files.readAllLines(path))
    val module = ScalaParser.parse(content)
    val interpreter = new Interpreter()
    val coreVisitor = new CoreVisitor()

    val coreModule = coreVisitor.transformModule(module)

    coreModule.accept(interpreter)
    assert(module.name == "SimpleRangeCaseModule")

    assert(interpreter.env.lookup("xs") == Some(IntValue(20)));
  }

  ignore("Testing the StmtCaseCore04 expressions after conversion to IfElse") {
    val path = Paths.get(getClass.getClassLoader.getResource("stmts/StmtCaseCore04.oberon").toURI)

    assert(path != null)

    val content = String.join("\n", Files.readAllLines(path))
    val module = ScalaParser.parse(content)
    val coreVisitor = new CoreVisitor()

    val coreModule = coreVisitor.transformModule(module)

    assert(coreModule.name == "SimpleRangeCaseModule")
    assert (coreModule.stmt.isDefined)
    // assert that the main block contains a sequence of statements
    module.stmt.get match {
      case SequenceStmt(stmts) => assert(stmts.length == 5)
      case _ => fail("we are expecting five stmts in the main block")
    }
    // now we can assume that the main block contains a sequence of stmts
    val sequence = coreModule.stmt.get.asInstanceOf[SequenceStmt]
    val stmts = sequence.stmts

    assert(stmts.head == AssignmentStmt(VarAssignment("xs"), IntValue(12)))
    assert(stmts(1) == AssignmentStmt(VarAssignment("min"), IntValue(10)))
    assert(stmts(2) == AssignmentStmt(VarAssignment("max"), IntValue(20)))
    assert(stmts(3) == IfElseStmt(EQExpression(VarExpression("xs"), IntValue(1)),
      AssignmentStmt(VarAssignment("xs"), IntValue(5)),
      Some(IfElseStmt(EQExpression(VarExpression("xs"), IntValue(2)),
        AssignmentStmt(VarAssignment("xs"), IntValue(10)),
        Some(IfElseStmt(AndExpression(LTEExpression(VarExpression("min"), VarExpression("xs")), LTEExpression(VarExpression("xs"), VarExpression("max"))),
          AssignmentStmt(VarAssignment("xs"), IntValue(20)),
          Some(AssignmentStmt(VarAssignment("xs"), IntValue(0)))))))
    ))
    assert((stmts(4) == WriteStmt(VarExpression("xs"))))
  }
  /** ###### Case Tests end here ###### */

  /** ###### Case Tests for CoreChecker ###### */
  ignore("Testing if Core for valid Core for StmtCaseCore04") {
    val path = Paths.get(getClass.getClassLoader.getResource("stmts/StmtCaseCore04.oberon").toURI)

    assert(path != null)

    val coreVisitor = new CoreVisitor()
    val content = String.join("\n", Files.readAllLines(path))

    val module = ScalaParser.parse(content)
    val isCore = CoreChecker.isModuleCore(module)

    val coreModule = coreVisitor.transformModule(module)
    val isCore2 = CoreChecker.isModuleCore(coreModule)

    assert(!isCore)
    assert(isCore2)
  }

  ignore("Testing if Core for valid Core for loop_stmt01") {
    val path = Paths.get(getClass.getClassLoader.getResource("stmts/loop_stmt01.oberon").toURI)

    assert(path != null)

    val coreVisitor = new CoreVisitor()
    val content = String.join("\n", Files.readAllLines(path))

    val module = ScalaParser.parse(content)
    val isCore = CoreChecker.isModuleCore(module)

    val coreModule = coreVisitor.transformModule(module)
    val isCore2 = CoreChecker.isModuleCore(coreModule)

    assert(!isCore)
    assert(isCore2)
  }

  ignore("Testing if Core for valid Core for RepeatUntilStmt06") {
    val path = Paths.get(getClass.getClassLoader.getResource("stmts/RepeatUntilStmt06.oberon").toURI)

    assert(path != null)

    val coreVisitor = new CoreVisitor()
    val content = String.join("\n", Files.readAllLines(path))

    val module = ScalaParser.parse(content)
    val isCore = CoreChecker.isModuleCore(module)

    val coreModule = coreVisitor.transformModule(module)
    val isCore2 = CoreChecker.isModuleCore(coreModule)

    assert(!isCore)
    assert(isCore2)
  }

  ignore("Testing if Core for valid Core for RepeatUntil04") {
    val path = Paths.get(getClass.getClassLoader.getResource("stmts/repeatuntil04.oberon").toURI)

    assert(path != null)

    val coreVisitor = new CoreVisitor()
    val content = String.join("\n", Files.readAllLines(path))

    val module = ScalaParser.parse(content)
    val isCore = CoreChecker.isModuleCore(module)

    val coreModule = coreVisitor.transformModule(module)
    val isCore2 = CoreChecker.isModuleCore(coreModule)

    assert(!isCore)
    assert(isCore2)
  }
}
