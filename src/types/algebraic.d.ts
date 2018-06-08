export interface Setoid<T> {
  /**
   * `equals :: Setoid a -> Setoid a -> Bool`
   */
  equals(x: Setoid<T>): boolean;
}

export interface Ord<T> extends Setoid<T> {
  /**
   * `lte :: Ord a -> Ord a -> Bool`
   */
  lte<T extends string | number>(this: Ord<T>, x: Ord<T>): boolean;
}

export interface Semigroup<T> {
  /**
   * `concat :: Semigroup a -> Semigroup a -> Semigroup a`
   */
  concat(x: Semigroup<T>): Semigroup<T>;
}

export interface Functor<T> {
  /**
   * `map :: Functor f => f a ~> (a -> b) -> f b`
   */
  map<U>(fn: (t: T) => U): Functor<U>;
}

export interface Apply<T> extends Functor<T> {
  /**
   * `ap :: Apply f => f a ~> f (a -> b) -> f b`
   */
  ap<U>(m: Apply<(value: T) => U>): Apply<U>;
}

export interface Applicative<T> extends Apply<T> {
  /**
   * `of :: Applicative f => a -> f a`
   * @static
   */
  of<U>(t: U): Applicative<U>;
}

export interface Foldable<T> {
  /**
   * `foldl :: (b -> a -> b) -> b -> Foldable a -> b`
   */
  foldl<U>(fn: (acc: U) => (current: T) => U): (initial: U) => U;
}

export interface Bind<T> extends Apply<T> {
  /**
   * `chain :: Bind m => m a ~> (a -> m b) -> m b`
   */
  bind<U>(fn: (t: T) => Bind<U>): Bind<U>;
}

export interface ChainRec<T> extends Bind<T> {
  /**
   * `chainRec :: ChainRec m => ((a -> c, b -> c, a) -> m c, a) -> m b`
   * @static
   */
  chainRec<T, U, V>(fn: (fn1: (x: T) => V, fn2: (x: U) => V, x: T) => ChainRec<V>, x: T): ChainRec<U>;
}