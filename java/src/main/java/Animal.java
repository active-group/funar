public interface Animal {
    public record Dillo(Liveness liveness, double weight)
            implements Animal {
        // Open/Closed: einfach, neue Tierarten hinzuzufügen
        // teuer, neue Operationen hinzuzufügen
        @Override
        public Animal runOver() {
            return new Dillo(Liveness.DEAD, this.weight);
        }
    }
    public record Parrot(String sentence, double weight)
            implements Animal {
        @Override
        public Animal runOver() {
            return new Parrot("", this.weight);
        }
    }

    Animal runOver();
    /*
    default public Animal runOver() {
    // Open/Closed: teuer, neue Tierarten hinzuzufügen
    // billig, neue Funktionen hinzuzufügen
        return switch (this) {
            case Dillo(var liveness, var weight) ->
                new Dillo(Liveness.DEAD, weight);
            case Parrot(var sentence, var weight) ->
                new Parrot("", weight);
        };
    }*/
}

// beides billig: Expression Problem