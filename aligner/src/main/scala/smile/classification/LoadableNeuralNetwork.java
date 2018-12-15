package smile.classification;


import smile.math.Math;

/*
The point of this class that I can load it really quickly
 */
public class LoadableNeuralNetwork implements SoftClassifier<double[]> {
    /**
     * A layer of a feed forward neural network.
     */
    private class Layer {
        /**
         * number of units in this layer
         */
        int units;
        /**
         * output of i<i>th</i> unit
         */
        double[] output;

        /**
         * connection weights to i<i>th</i> unit from previous layer
         */
        double[][] weight;
    }

    /**
     * The type of activation function in output layer.
     */
    private final NeuralNetwork.ActivationFunction activationFunction = NeuralNetwork.ActivationFunction.LOGISTIC_SIGMOID;

    public LoadableNeuralNetwork(int inputSize, double[][][] weights) {
        net = new Layer[weights.length + 1];
        Layer input = new Layer();
        input.units = inputSize;
        input.output = new double[inputSize + 1];
        net[0] = input;
        for(int i = 0; i < weights.length; ++i) {
            double[][] layerWeights = weights[i];
            int numUnits = layerWeights.length;
            Layer layer = new Layer();
            layer.units = numUnits;
            layer.output = new double[numUnits + 1];
            layer.weight = layerWeights;
            net[i + 1] = layer;
        }
        inputLayer = net[0];
        outputLayer = net[net.length - 1];
    }
    /**
     * layers of this net
     */
    private final LoadableNeuralNetwork.Layer[] net;
    /**
     * input layer
     */
    private final LoadableNeuralNetwork.Layer inputLayer;
    /**
     * output layer
     */
    private final LoadableNeuralNetwork.Layer outputLayer;


    /**
     * Sets the input vector into the input layer.
     * @param x the input vector.
     */
    private void setInput(double[] x) {
        if (x.length != inputLayer.units) {
            throw new IllegalArgumentException(String.format("Invalid input vector size: %d, expected: %d", x.length, inputLayer.units));
        }
        System.arraycopy(x, 0, inputLayer.output, 0, inputLayer.units);
    }

    /**
     * Returns the output vector into the given array.
     * @param y the output vector.
     */
    private void getOutput(double[] y) {
        if (y.length != outputLayer.units) {
            throw new IllegalArgumentException(String.format("Invalid output vector size: %d, expected: %d", y.length, outputLayer.units));
        }
        System.arraycopy(outputLayer.output, 0, y, 0, outputLayer.units);
    }

    /**
     * Propagates signals from a lower layer to the next upper layer.
     * @param lower the lower layer where signals are from.
     * @param upper the upper layer where signals are propagated to.
     */
    private void propagate(LoadableNeuralNetwork.Layer lower, LoadableNeuralNetwork.Layer upper) {
        for (int i = 0; i < upper.units; i++) {
            double sum = 0.0;
            for (int j = 0; j <= lower.units; j++) {
                sum += upper.weight[i][j] * lower.output[j];
            }

            if (upper != outputLayer || activationFunction == NeuralNetwork.ActivationFunction.LOGISTIC_SIGMOID) {
                upper.output[i] = Math.logistic(sum);
            } else {
                if (activationFunction == NeuralNetwork.ActivationFunction.LINEAR || activationFunction == NeuralNetwork.ActivationFunction.SOFTMAX) {
                    upper.output[i] = sum;
                } else {
                    throw new UnsupportedOperationException("Unsupported activation function.");
                }
            }
        }

        if (upper == outputLayer && activationFunction == NeuralNetwork.ActivationFunction.SOFTMAX) {
            softmax();
        }
    }

    /**
     * Calculate softmax activation function in output layer without overflow.
     */
    private void softmax() {
        double max = Double.NEGATIVE_INFINITY;
        for (int i = 0; i < outputLayer.units; i++) {
            if (outputLayer.output[i] > max) {
                max = outputLayer.output[i];
            }
        }

        double sum = 0.0;
        for (int i = 0; i < outputLayer.units; i++) {
            double out = Math.exp(outputLayer.output[i] - max);
            outputLayer.output[i] = out;
            sum += out;
        }

        for (int i = 0; i < outputLayer.units; i++) {
            outputLayer.output[i] /= sum;
        }
    }

    /**
     * Propagates the signals through the neural network.
     */
    private void propagate() {
        for (int l = 0; l < net.length - 1; l++) {
            propagate(net[l], net[l + 1]);
        }
    }


    /**
     * Predict the target value of a given instance. Note that this method is NOT
     * multi-thread safe.
     * @param x the instance.
     * @param y the array to store network output on output. For softmax
     * activation function, these are estimated posteriori probabilities.
     * @return the predicted class label.
     */
    @Override
    public int predict(double[] x, double[] y) {
        setInput(x);
        propagate();
        getOutput(y);

        if (outputLayer.units == 1) {
            if (outputLayer.output[0] > 0.5) {
                return 0;
            } else {
                return 1;
            }
        }

        double max = Double.NEGATIVE_INFINITY;
        int label = -1;
        for (int i = 0; i < outputLayer.units; i++) {
            if (outputLayer.output[i] > max) {
                max = outputLayer.output[i];
                label = i;
            }
        }
        return label;
    }

    /**
     * Predict the class of a given instance. Note that this method is NOT
     * multi-thread safe.
     * @param x the instance.
     * @return the predicted class label.
     */
    @Override
    public int predict(double[] x) {
        setInput(x);
        propagate();

        if (outputLayer.units == 1) {
            if (outputLayer.output[0] > 0.5) {
                return 0;
            } else {
                return 1;
            }
        }

        double max = Double.NEGATIVE_INFINITY;
        int label = -1;
        for (int i = 0; i < outputLayer.units; i++) {
            if (outputLayer.output[i] > max) {
                max = outputLayer.output[i];
                label = i;
            }
        }
        return label;
    }

}
