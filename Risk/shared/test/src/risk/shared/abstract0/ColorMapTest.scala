package risk.shared.abstract0

import org.scalatest.funsuite.AnyFunSuite

class ColorMapTest extends AnyFunSuite {

  def invariant(cm: ColorMap): Unit = {
    assert(cm.colorMap.values.toList.distinct.size == cm.colorMap.size)
  }

  test("roc1") {
    val cm = ColorMap()
    assert(cm.roleOfColor(Green).isEmpty)
    assert(cm.colorOfRole(1).isEmpty)
  }

  test("roc2") {
    val cm = ColorMap()
    val cm2 = cm.addOrUpdateColor(Green, 1).addOrUpdateColor(Red, 4)
    assert(cm2.roleOfColor(Red).contains(4))
    assert(cm2.roleOfColor(Green).contains(1))
    assert(cm2.roleOfColor(Blue).isEmpty)
    assert(cm2.roleOfColor(White).isEmpty)

    assert(cm2.colorOfRole(4).contains(Red))
    assert(cm2.colorOfRole(1).contains(Green))
    assert(cm2.colorOfRole(17).isEmpty)
  }

  test("adding the first element") {
    val cm = ColorMap()
    invariant(cm)
    val cm1 = cm.addOrUpdateColor(Green, 1)
    invariant(cm1)
    assert(cm1.colorMap.size == 1)
    assert(cm1.colorMap(Green) == 1)
  }

  test("updating color") {
    val cm = ColorMap()
    invariant(cm)
    val cm1 = cm.addOrUpdateColor(Green, 1)
    invariant(cm1)
    assert(cm1.colorMap.size == 1)
    assert(cm1.colorMap(Green) == 1)

    val cm2 = cm1.addOrUpdateColor(Green, 3)
    invariant(cm2)
    assert(cm2.colorMap.size == 1)
    assert(cm2.colorMap(Green) == 3)
  }

  test("a role id cannot have more than one color") {
    val cm = ColorMap()
    invariant(cm)
    val cm1 = cm.addOrUpdateColor(Green, 1)
    invariant(cm1)
    assert(cm1.colorMap.size == 1)
    assert(cm1.colorMap(Green) == 1)

    val cm2 = cm1.addOrUpdateColor(Red, 1)
    invariant(cm2)
    assert(cm2.colorMap(Red) == 1)
    assert(cm2.colorMap.size == 1)
  }
}
