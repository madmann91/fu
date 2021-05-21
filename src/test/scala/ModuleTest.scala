import org.junit.Test
import org.junit.Assert.*
import eta.print

class ModuleTest {
  @Test def hashCons() = {
    val module = eta.Module()
    val oldCount = module.nodeCount
    assertEquals(module.nat, module.nat)
    assertEquals(module.createNatLit(1), module.createNatLit(1))
    assertEquals(
      module.createFloatLit(module.createSizedFloat(32), 1.0),
      module.createFloatLit(module.createSizedFloat(32), 1.0))
    assertNotEquals(
      module.createFloatLit(module.createSizedFloat(16), 1.0),
      module.createFloatLit(module.createSizedFloat(32), 1.0))
    assertEquals(
      module.createIntLit(module.createSizedInt(32), 1),
      module.createIntLit(module.createSizedInt(32), 1))
    assertNotEquals(
      module.createIntLit(module.createSizedInt(16), 1),
      module.createIntLit(module.createSizedInt(32), 1))
    val expectedCount = oldCount +
      3 + // Nat literals: 1, 16, 32
      4 + // Types: Float16, Float32, Int16, Int32
      4   // Other literals: 1.0: Float16, 1.0: Float32, 1: Int16, 1: Int32
    assertEquals(module.nodeCount, expectedCount)
  }
}
