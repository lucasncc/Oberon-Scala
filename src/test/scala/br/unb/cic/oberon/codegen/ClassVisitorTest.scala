package br.unb.cic.oberon.codegen

import jdk.internal.org.objectweb.asm.{ClassVisitor, FieldVisitor}
import jdk.internal.org.objectweb.asm.Opcodes._;
//import org.objectweb.asm._

class ClassVisitorTest(version: Integer) extends ClassVisitor(version) {
  var numberOfFields: Integer = 0;
  var numberOfIntegerVariables: Integer = 0;
  var numberOfBooleanVariables: Integer = 0;
  var numberOfIntegerConstants: Integer = 0;
  var numberOfBooleanConstants: Integer = 0;

  override def visitField(access: Int, name: String, desc: String, signature: String, value: Any): FieldVisitor = {
    numberOfFields += 1;

    access match {
      case ACC_PUBLIC => {
        desc match {
          case "I" => {
            numberOfIntegerVariables += 1;
          }
          case "Z" => {
            numberOfBooleanVariables += 1;
          }
        }
      }
      case (Integer.valueOf(ACC_PUBLIC) + Integer.valueOf(ACC_FINAL)) => {
        desc match {
          case "I" => {
            numberOfIntegerConstants += 1;
          }
          case "Z" => {
            numberOfBooleanConstants += 1;
          }
        }
      }
    }

    super.visitField(access, name, desc, signature, value)
  }
}