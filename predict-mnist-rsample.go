package main

import (
	"bufio"
	"fmt"
	"math/rand"
	"os"
	"strconv"
	"strings"
)

func square(x float64) float64 {
	return x * x
}

func sumSqErr(xs []float64, ys []float64, start int) float64 {
	s := 0.0
	for i := start; i < len(xs); i++ {
		s += square(xs[i] - ys[i])
	}
	return s
}

func sumSqErrSample(xs []float64, ys []float64, start int, sampleIndexes []int) float64 {
	s := 0.0
	for _, i := range sampleIndexes {
		s += square(xs[i] - ys[i])
	}
	return s
}

func predict(trainData [][]float64, test []float64, nensemble int, nsample int) float64 {
	votes := make(map[float64]int)

	for sampleNum := 0; sampleNum < nensemble; sampleNum++ {
		predicted := 0.0
		minError := -1.0

		// Create a shuffled slice of indices from 1 to len(test)-1
		indices := rand.Perm(len(test) - 1)
		sampleIndexes := make([]int, nsample)
		for i := 0; i < nsample; i++ {
			sampleIndexes[i] = indices[i] + 1 // Adjust index to be 1-based
		}

		for _, train := range trainData {
			error := sumSqErrSample(test, train, 1, sampleIndexes)
			if error < minError || minError < 0 {
				predicted = train[0]
				minError = error
			}
		}
		votes[predicted]++
	}

	// Find max vote
	maxk := 0.0
	maxv := 0
	for k, v := range votes {
		if v > maxv {
			maxk = k
			maxv = v
		}
	}
	return maxk
}

func main() {
	fileNameTrain := "mnist_train.csv"
	fileNameTest := "mnist_test.csv"

	// train
	trainFile, err := os.Open(fileNameTrain)
	if err != nil {
		fmt.Println("Error opening train file:", err)
		return
	}
	defer trainFile.Close()

	var trainData [][]float64
	scanner := bufio.NewScanner(trainFile)
	for scanner.Scan() {
		line := scanner.Text()
		fields := strings.Split(line, ",")
		var row []float64
		for _, field := range fields {
			val, err := strconv.ParseFloat(field, 64)
			if err != nil {
				fmt.Println("Error parsing float in train data:", err)
				return
			}
			row = append(row, val)
		}
		trainData = append(trainData, row)
	}

	if err := scanner.Err(); err != nil {
		fmt.Println("Error reading train file:", err)
		return
	}
	fmt.Printf("train data loaded. rows: %d\n", len(trainData))

	// test
	testFile, err := os.Open(fileNameTest)
	if err != nil {
		fmt.Println("Error opening test file:", err)
		return
	}
	defer testFile.Close()

	nRows := 0
	nCorrect := 0
	testScanner := bufio.NewScanner(testFile)
	for testScanner.Scan() {
		if err := testScanner.Err(); err != nil {
			fmt.Println("Error reading test file:", err)
			return
		}
		line := testScanner.Text()
		fields := strings.Split(line, ",")
		var test []float64
		for _, field := range fields {
			val, err := strconv.ParseFloat(field, 64)
			if err != nil {
				fmt.Println("Error parsing float in test data:", err)
				return
			}
			test = append(test, val)
		}
		nRows++
		predicted := predict(trainData, test, 10, 600)
		answer := test[0]
		if predicted == answer {
			nCorrect++
		}
		accuracy := float64(nCorrect) / float64(nRows)
		fmt.Printf("row: %d, predicted: %.1f, answer: %.1f, accuracy: %.4f\n", nRows, predicted, answer, accuracy)
	}
}
