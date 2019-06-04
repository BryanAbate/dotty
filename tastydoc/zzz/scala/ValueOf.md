scala
# class ValueOf

## Companion object ValueOf

<pre><code class="language-scala" >final class ValueOf[T] extends AnyVal</pre></code>
`ValueOf[T]` provides the unique value of the type `T` where `T` is a type which has a
single inhabitant. Eligible types are singleton types of the form `stablePath.type`,
Unit and singleton types corresponding to value literals.
Instances of `ValueOf[T]` are provided implicitly for all eligible types. Typically
an instance would be required where a runtime value corresponding to a type level
computation is needed.
For example, we might define a type `Residue[M <: Int]` corresponding to the group of
integers modulo `M`. We could then mandate that residues can be summed only when they
are parameterized by the same modulus,
```scala
case class Residue[M <: Int](n: Int) extends AnyVal {
 def +(rhs: Residue[M])(implicit m: ValueOf[M]): Residue[M] =
   Residue((this.n + rhs.n) % valueOf[M])
}

val fiveModTen = Residue[10](5)
val nineModTen = Residue[10](9)

fiveModTen + nineModTen    // OK == Residue[10](4)

val fourModEleven = Residue[11](4)

fiveModTen + fourModEleven // compiler error: type mismatch;
                          //   found   : Residue[11]
                          //   required: Residue[10]
```
Notice that here the modulus is encoded in the type of the values and so does not
incur any additional per-value storage cost. When a runtime value of the modulus
is required in the implementation of `+` it is provided at the call site via the
implicit argument `m` of type `ValueOf[M]`.

## Annotations:
@implicitNotFound 
## Constructors:
<pre><code class="language-scala" >ValueOf(value: T)</pre></code>

## Concrete Value Members:
### !=
<pre><code class="language-scala" >final def !=(x$0: Any): Boolean</pre></code>

### ##
<pre><code class="language-scala" >final def ##: Int</pre></code>

### ==
<pre><code class="language-scala" >final def ==(x$0: Any): Boolean</pre></code>

### asInstanceOf
<pre><code class="language-scala" >final def asInstanceOf[X0]: X0</pre></code>

### getClass
<pre><code class="language-scala" >final def getClass(): Class[Nothing <: Any]</pre></code>

### isInstanceOf
<pre><code class="language-scala" >final def isInstanceOf[X0]: Boolean</pre></code>

### toString
<pre><code class="language-scala" >def toString(): String</pre></code>

### value
<pre><code class="language-scala" >val value: <a href="./ValueOf.md#T">T</a></pre></code>
