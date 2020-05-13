package de.ag;

import java.util.function.Function;

public interface Functor<F, T> { // Generics in Java haben immer Kind *
    <U> F<U> universalMap(Function<T, U> f, F<T> c);
}
