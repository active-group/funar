package de.ag;

import java.util.Objects;

public class Parrot implements Animal {
    final String sentence;
    final int weight;

    public Parrot(String sentence, int weight) {
        this.sentence = sentence;
        this.weight = weight;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Parrot parrot = (Parrot) o;
        return weight == parrot.weight &&
                Objects.equals(sentence, parrot.sentence);
    }

    @Override
    public int hashCode() {
        return Objects.hash(sentence, weight);
    }

    @Override
    public Animal runOver() {
        return new Parrot("", this.weight);
    }

    @Override
    public Animal feed(int amount) {
        return new Parrot(this.sentence, this.weight + amount);
    }
}
