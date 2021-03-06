scala.IArray$package$
# object IArray

<pre><code class="language-scala" >final object IArray extends Serializable</pre></code>
## Concrete Value Members:
### apply
<pre><code class="language-scala" >def apply(x: Unit, xs: Seq[Unit]): IArray[Unit]</pre></code>

### apply
<pre><code class="language-scala" >def apply(x: Double, xs: Seq[Double]): IArray[Double]</pre></code>

### apply
<pre><code class="language-scala" >def apply(x: Float, xs: Seq[Float]): IArray[Float]</pre></code>

### apply
<pre><code class="language-scala" >def apply(x: Long, xs: Seq[Long]): IArray[Long]</pre></code>

### apply
<pre><code class="language-scala" >def apply(x: Int, xs: Seq[Int]): IArray[Int]</pre></code>

### apply
<pre><code class="language-scala" >def apply(x: Char, xs: Seq[Char]): IArray[Char]</pre></code>

### apply
<pre><code class="language-scala" >def apply(x: Short, xs: Seq[Short]): IArray[Short]</pre></code>

### apply
<pre><code class="language-scala" >def apply(x: Byte, xs: Seq[Byte]): IArray[Byte]</pre></code>

### apply
<pre><code class="language-scala" >def apply(x: Boolean, xs: Seq[Boolean]): IArray[Boolean]</pre></code>

### apply
<pre><code class="language-scala" >def apply[T](xs: Seq[T])(implicit evidence$4: <a href="../reflect/ClassTag.md">ClassTag</a>[T]): IArray[T]</pre></code>
An immutable array with given elements.

### concat
<pre><code class="language-scala" >def concat[T](xss: Seq[IArray[T]])(implicit evidence$5: <a href="../reflect/ClassTag.md">ClassTag</a>[T]): IArray[T]</pre></code>
Concatenates all arrays into a single immutable array.

***return*** the array created from concatenating `xss`

***xss*** the given immutable arrays

### empty
<pre><code class="language-scala" >def empty[T](implicit evidence$3: <a href="../reflect/ClassTag.md">ClassTag</a>[T]): IArray[T]</pre></code>
An immutable array of length 0.

### fill
<pre><code class="language-scala" >def fill[T](n1: Int, n2: Int, n3: Int, n4: Int, n5: Int)(elem: => T)(implicit evidence$10: <a href="../reflect/ClassTag.md">ClassTag</a>[T]): IArray[IArray[IArray[IArray[IArray[T]]]]]</pre></code>
Returns a five-dimensional immutable array that contains the results of some element computation a number
of times. Each element is determined by a separate computation.

***n2*** the number of elements in the 2nd dimension

***n5*** the number of elements in the 5th dimension

***n1*** the number of elements in the 1st dimension

***n4*** the number of elements in the 4th dimension

***elem*** the element computation

***n3*** the number of elements in the 3nd dimension

### fill
<pre><code class="language-scala" >def fill[T](n1: Int, n2: Int, n3: Int, n4: Int)(elem: => T)(implicit evidence$9: <a href="../reflect/ClassTag.md">ClassTag</a>[T]): IArray[IArray[IArray[IArray[T]]]]</pre></code>
Returns a four-dimensional immutable array that contains the results of some element computation a number
of times. Each element is determined by a separate computation.

***n2*** the number of elements in the 2nd dimension

***n1*** the number of elements in the 1st dimension

***n4*** the number of elements in the 4th dimension

***elem*** the element computation

***n3*** the number of elements in the 3nd dimension

### fill
<pre><code class="language-scala" >def fill[T](n1: Int, n2: Int, n3: Int)(elem: => T)(implicit evidence$8: <a href="../reflect/ClassTag.md">ClassTag</a>[T]): IArray[IArray[IArray[T]]]</pre></code>
Returns a three-dimensional immutable array that contains the results of some element computation a number
of times. Each element is determined by a separate computation.

***n2*** the number of elements in the 2nd dimension

***n3*** the number of elements in the 3nd dimension

***n1*** the number of elements in the 1st dimension

***elem*** the element computation

### fill
<pre><code class="language-scala" >def fill[T](n1: Int, n2: Int)(elem: => T)(implicit evidence$7: <a href="../reflect/ClassTag.md">ClassTag</a>[T]): IArray[IArray[T]]</pre></code>
Returns a two-dimensional immutable array that contains the results of some element computation a number
of times. Each element is determined by a separate computation.

***n2*** the number of elements in the 2nd dimension

***n1*** the number of elements in the 1st dimension

***elem*** the element computation

### fill
<pre><code class="language-scala" >def fill[T](n: Int)(elem: => T)(implicit evidence$6: <a href="../reflect/ClassTag.md">ClassTag</a>[T]): IArray[T]</pre></code>
Returns an immutable array that contains the results of some element computation a number
of times. Each element is determined by a separate computation.

***n*** the number of elements in the array

***elem*** the element computation

### iterate
<pre><code class="language-scala" >def iterate[T](start: T, len: Int)(f: (T) => T)(implicit evidence$16: <a href="../reflect/ClassTag.md">ClassTag</a>[T]): IArray[T]</pre></code>
Returns an immutable array containing repeated applications of a function to a start value.

***return*** the immutable array returning `len` values in the sequence `start, f(start), f(f(start)), ...`

***f*** the function that is repeatedly applied

***len*** the number of elements returned by the array

***start*** the start value of the array

### range
<pre><code class="language-scala" >def range(start: Int, end: Int, step: Int): IArray[Int]</pre></code>
Returns an immutable array containing equally spaced values in some integer interval.

***return*** the immutable array with values in `start, start + step, ...` up to, but excluding `end`

***step*** the increment value of the array (may not be zero)

***end*** the end value of the array, exclusive (in other words, this is the first value **not** returned)

***start*** the start value of the array

### range
<pre><code class="language-scala" >def range(start: Int, end: Int): IArray[Int]</pre></code>
Returns an immutable array containing a sequence of increasing integers in a range.

***return*** the immutable array with values in range `start, start + 1, ..., end - 1`
up to, but excluding, `end`.

***end*** the end value of the array, exclusive (in other words, this is the first value **not** returned)

***start*** the start value of the array

### tabulate
<pre><code class="language-scala" >def tabulate[T](n1: Int, n2: Int, n3: Int, n4: Int, n5: Int)(f: (Int, Int, Int, Int, Int) => T)(implicit evidence$15: <a href="../reflect/ClassTag.md">ClassTag</a>[T]): IArray[IArray[IArray[IArray[IArray[T]]]]]</pre></code>
Returns a five-dimensional immutable array containing values of a given function
over ranges of integer values starting from `0`.

***n2*** the number of elements in the 2nd dimension

***n5*** the number of elements in the 5th dimension

***n1*** the number of elements in the 1st dimension

***f*** The function computing element values

***n4*** the number of elements in the 4th dimension

***n3*** the number of elements in the 3rd dimension

### tabulate
<pre><code class="language-scala" >def tabulate[T](n1: Int, n2: Int, n3: Int, n4: Int)(f: (Int, Int, Int, Int) => T)(implicit evidence$14: <a href="../reflect/ClassTag.md">ClassTag</a>[T]): IArray[IArray[IArray[IArray[T]]]]</pre></code>
Returns a four-dimensional immutable array containing values of a given function
over ranges of integer values starting from `0`.

***n2*** the number of elements in the 2nd dimension

***n1*** the number of elements in the 1st dimension

***f*** The function computing element values

***n4*** the number of elements in the 4th dimension

***n3*** the number of elements in the 3rd dimension

### tabulate
<pre><code class="language-scala" >def tabulate[T](n1: Int, n2: Int, n3: Int)(f: (Int, Int, Int) => T)(implicit evidence$13: <a href="../reflect/ClassTag.md">ClassTag</a>[T]): IArray[IArray[IArray[T]]]</pre></code>
Returns a three-dimensional immutable array containing values of a given function
over ranges of integer values starting from `0`.

***n2*** the number of elements in the 2nd dimension

***f*** The function computing element values

***n3*** the number of elements in the 3rd dimension

***n1*** the number of elements in the 1st dimension

### tabulate
<pre><code class="language-scala" >def tabulate[T](n1: Int, n2: Int)(f: (Int, Int) => T)(implicit evidence$12: <a href="../reflect/ClassTag.md">ClassTag</a>[T]): IArray[IArray[T]]</pre></code>
Returns a two-dimensional immutable array containing values of a given function
over ranges of integer values starting from `0`.

***n2*** the number of elements in the 2nd dimension

***f*** The function computing element values

***n1*** the number of elements in the 1st dimension

### tabulate
<pre><code class="language-scala" >def tabulate[T](n: Int)(f: (Int) => T)(implicit evidence$11: <a href="../reflect/ClassTag.md">ClassTag</a>[T]): IArray[T]</pre></code>
Returns an immutable array containing values of a given function over a range of integer
values starting from 0.

***n*** The number of elements in the array

***f*** The function computing element values

### unapplySeq
<pre><code class="language-scala" >def unapplySeq[T](x: IArray[T]): Option[IndexedSeq[T]]</pre></code>
Returns a decomposition of the array into a sequence. This supports
a pattern match like `{ case IArray(x,y,z) => println('3 elements')}`.

***return*** sequence wrapped in a scala.Some, if `x` is a Seq, otherwise `None`

***x*** the selector value

