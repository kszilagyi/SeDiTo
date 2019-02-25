
# TensorFlow and tf.keras
from keras.models import Sequential
from keras.layers import Dense

# Helper libraries
import numpy as np
import matplotlib.pyplot as plt
import csv

def load_csv(filename, size):
    # dataset = tf.data.experimental.CsvDataset(filename, [tf.float32] * size)
    # dataset.map(lambda x: tf.stack(list(x.values())))
    res = []
    with open(filename) as csvfile:
      reader = csv.reader(csvfile, delimiter=',', quoting=csv.QUOTE_NONNUMERIC)
      for row in reader:
        res.append(np.array(list(row)))
    return np.array(res)

# tf.enable_eager_execution()
#
def print_tensor(t):
    tf.print(t, [t])  # Here we are using the value returned by tf.Print


def count(results, reference_results, condition):
    res = 0
    assert(len(results) == len(reference_results))
    for i in range(len(results)):
        if condition(results[i], reference_results[i]):
            res = res + 1

    return res

def calc_tp(results, reference_results):
    return count(results, reference_results, lambda res, ref_res: res > 0.5 and ref_res > 0.5)

def calc_fp(results, reference_results):
    return count(results, reference_results, lambda res, ref_res: res > 0.5 and ref_res <= 0.5)

def calc_fn(results, reference_results):
    return count(results, reference_results, lambda res, ref_res: res <= 0.5 and ref_res > 0.5)

def calc_precision(true_positives, all_positives):
    return true_positives / all_positives

def calc_recall(true_positives, real_positives):
    return true_positives / real_positives

def calc_f1(precision, recall):
    return 2 * (precision * recall) / (precision + recall)

def calc_metrics(results, reference_results):
    tp = calc_tp(results, reference_results)
    fp = calc_fp(results, reference_results)
    fn = calc_fn(results, reference_results)
    precision = calc_precision(tp, all_positives=tp + fp)
    recall = calc_recall(tp, real_positives=tp+fn)
    f1 = calc_f1(precision, recall)
    return {'tp': tp, 'fp': fp, 'fn': fn, 'precision': precision, 'recall': recall, 'f1': f1}

test_features = load_csv('test-features.csv', 102)
test_labels = load_csv('test-labels.csv', 1)
train_features = load_csv('training-features.csv', 102)
train_labels = load_csv('training-labels.csv', 1)
print('Loaded csvs')
print(test_features.shape)
print(test_labels.shape)
print(train_features.shape)
print(train_labels.shape)
# model = keras.Sequential([
#     keras.layers.Dense(128, activation=tf.nn.relu),
#     keras.layers.Dense(1, activation=tf.nn.softmax)
# ])
#
# model.compile(optimizer=AdamOptimizer(),
#               loss='binary_crossentropy',
#               metrics=['accuracy'])
# print('Compiled model')
# model.fit(train_features, train_labels, epochs=5, shuffle=False)
# print('fitted model')
# test_loss, test_acc = model.evaluate(test_features, test_labels)
# train_predict = model.predict(train_features)
# test_predict = model.predict(test_features)
#
# print('Test accuracy:', test_acc)
# model.save_weights('model.h5')
model = Sequential()
model.add(Dense(units=128, activation='relu', input_dim=102))
model.add(Dense(units=128, activation='relu'))
model.add(Dense(units=128, activation='relu'))
model.add(Dense(units=1, activation='sigmoid'))

model.compile(optimizer="sgd",
                         loss='binary_crossentropy',
                         metrics=['binary_accuracy'])
model.fit(train_features, train_labels, epochs=15, shuffle=False)

# model.load_weights('model.h5')
train_predict = model.predict(train_features)
test_predict = model.predict(test_features)
train_metrics = calc_metrics(train_predict, train_labels)
test_metrics = calc_metrics(test_predict, test_labels)
print('train %s' % str(train_metrics))
print('test %s' % str(test_metrics))




# print(list(zip(list(train_predict[0:100]), list(train_labels[0:100]))))
# train_f1s, _ = tf.contrib.metrics.f1_score(
#     train_labels,
#     train_predict)
#
# test_f1s, _ = tf.contrib.metrics.f1_score(
#     test_labels,
#     test_predict)
#
# print_tensor(train_f1s)
# print_tensor(test_f1s)
