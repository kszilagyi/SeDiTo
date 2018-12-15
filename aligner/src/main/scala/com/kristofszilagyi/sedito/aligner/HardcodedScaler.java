package com.kristofszilagyi.sedito.aligner;
import smile.feature.Scaler;

final public class HardcodedScaler extends Scaler {
  @SuppressWarnings("unused") //scala
  public HardcodedScaler() {
    super(true);
    lo = new double[]{2.0, 2.0, 0.0, 0.04, 0.9999999999999999, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
    hi = new double[]{35.0, 35.0, 0.48, 0.96, 36.0, 193.0, 193.0, 1.0, 1.0, 182.0, 25.0, 25.0, 1.0, 1.0, 25.0, 25.0, 25.0, 1.0, 1.0, 25.0, 12.0, 12.0, 1.0, 1.0, 12.0, 12.0, 12.0, 1.0, 1.0, 12.0, 6.0, 6.0, 1.0, 1.0, 6.0, 6.0, 6.0, 1.0, 1.0, 6.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0};
  }
}
       