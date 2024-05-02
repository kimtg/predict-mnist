import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class Mnist {

    static int square(int x) {
        return x * x;
    }

    static int sumSquaredError(List<Integer> x, List<Integer> y) {
        if (x.size() != y.size()) {
            throw new IllegalArgumentException("Lists must have the same size.");
        }

        int sum = 0;
        for (int i = 0; i < x.size(); i++) {
            sum += square(x.get(i) - y.get(i));
        }
        return sum;
    }

    public static void main(String[] args) {
        System.out.println("!");

        String fileNameTrain = "mnist_train.csv";
        String fileNameTest = "mnist_test.csv";

        // Load train data
        List<List<Integer>> trainData = new ArrayList<>();
        try (BufferedReader reader = new BufferedReader(new FileReader(fileNameTrain))) {
            String line;
            while ((line = reader.readLine()) != null) {
                String[] parts = line.split(",");
                List<Integer> data = new ArrayList<>();
                for (String part : parts) {
                    data.add(Integer.parseInt(part));
                }
                trainData.add(data);
            }
        } catch (IOException e) {
            e.printStackTrace();
            return;
        }
        System.out.println("Train data loaded. rows: " + trainData.size());

        // Test
        int nRows = 0;
        int nCorrect = 0;
        try (BufferedReader reader = new BufferedReader(new FileReader(fileNameTest))) {
            String line;
            while ((line = reader.readLine()) != null) {
                nRows++;
                String[] parts = line.split(",");
                List<Integer> testData = new ArrayList<>();
                for (String part : parts) {
                    testData.add(Integer.parseInt(part));
                }

                // Predict
                int best = 0;
                int minError = -1;
                for (List<Integer> d : trainData) {
                    int error = sumSquaredError(testData.subList(1, testData.size()), d.subList(1, d.size()));
                    if (minError < 0 || error < minError) {
                        minError = error;
                        best = d.get(0);
                    }
                }

                if (best == testData.get(0)) {
                    nCorrect++;
                }

                System.out.println("row: " + nRows + " predicted: " + best + " answer: " + testData.get(0) +
                        " accuracy: " + ((double) nCorrect / nRows));
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}

