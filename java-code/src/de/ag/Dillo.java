package de.ag;

import java.util.Objects;

public class Dillo implements Animal {
    final boolean alive;
    final int weight;

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Dillo dillo = (Dillo) o;
        return alive == dillo.alive &&
                weight == dillo.weight;
    }

    @Override
    public int hashCode() {
        return Objects.hash(alive, weight);
    }

    public Dillo(boolean alive, int weight) {
        this.alive = alive;
        this.weight = weight;
    }

    @Override
    public Animal runOver() {
        return new Dillo(false, this.weight);
    }

    @Override
    public Animal feed(int amount) {
        return new Dillo(this.alive, this.weight + amount);
    }
}
