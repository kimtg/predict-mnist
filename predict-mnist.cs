using System;
using System.Collections.Generic;
using System.IO;

namespace MNISTClassifier
{
    class Program
    {
        static int Square(int x)
        {
            return x * x;
        }

        static int SumSquaredError(List<int> x, List<int> y, int start)
        {
            if (x.Count != y.Count)
            {
                throw new ArgumentException("Lists must have the same size.");
            }

            int sum = 0;
            for (int i = start; i < x.Count; i++)
            {
                sum += Square(x[i] - y[i]);
            }

            return sum;
        }

        static void Main(string[] args)
        {
            Console.WriteLine("!");

            const string fileNameTrain = "mnist_train.csv";
            const string fileNameTest = "mnist_test.csv";

            // Load train data
            var trainData = new List<List<int>>();
            using (var reader = new StreamReader(fileNameTrain))
            {
                while (!reader.EndOfStream)
                {
                    var line = reader.ReadLine();
                    var values = line.Split(',');

                    var data = new List<int>();
                    foreach (var value in values)
                    {
                        data.Add(int.Parse(value));
                    }

                    trainData.Add(data);
                }
            }
            Console.WriteLine($"Train data loaded. rows: {trainData.Count}");

            // Test
            using (var reader = new StreamReader(fileNameTest))
            {
                int nRows = 0;
                int nCorrect = 0;

                while (!reader.EndOfStream)
                {
                    nRows++;

                    var line = reader.ReadLine();
                    var values = line.Split(',');

                    var testData = new List<int>();
                    foreach (var value in values)
                    {
                        testData.Add(int.Parse(value));
                    }

                    // Predict
                    int best = 0;
                    int minError = -1;
                    foreach (var data in trainData)
                    {
                        var error = SumSquaredError(testData, data, 1);
                        if (minError < 0 || error < minError)
                        {
                            minError = error;
                            best = data[0];
                        }
                    }

                    if (best == testData[0])
                    {
                        nCorrect++;
                    }

                    Console.WriteLine($"row: {nRows} predicted: {best} answer: {testData[0]} accuracy: {(double)nCorrect / nRows}");
                }
            }
        }
    }
}
