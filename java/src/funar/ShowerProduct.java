package funar;

sealed public interface ShowerProduct {
    double soapProportion();

    default double soapProportion2() {
        return switch (this) {
            case Soap(var PH) -> 1;
            case Shampoo(var hairType) -> 0;
            case Mixture(var product1, var product2) ->
                    (product1.soapProportion2() + product2.soapProportion2())
                            / 2.0;
        };
    }
}

record Soap(double PH) implements ShowerProduct {

    @Override
    public double soapProportion() {
        return 1;
    }
}

record Shampoo(Hairtype hairType) implements ShowerProduct {
    @Override
    public double soapProportion() {
        return 1;
    }
}

record Mixture(ShowerProduct product1, ShowerProduct product2) implements ShowerProduct {

    @Override
    public double soapProportion() {
        return (this.product1.soapProportion() + this.product2.soapProportion())
                / 2.0;
    }
}

enum Hairtype {
    OILY, DANDRUFF, REGUALR
}
