// squared error, int
// nearest number
// accuracy: 0.65

package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

func square(x int) int {
	return x * x
}

func sumSqErr(x, y []int) int {
	l := len(x)
	if l != len(y) {
		log.Fatal("not same size")
	}
	sum := 0
	for i := range x {
		sum += square(x[i] - y[i])
	}
	return sum
}

func main() {
	fmt.Println("!")
	const fileNameTrain = "mnist_train.csv"
	const fileNameTest = "mnist_test.csv"

	// train
	file, err := os.Open(fileNameTrain)
	if err != nil {
		log.Fatal(err)
	}
	sin := bufio.NewScanner(file)
	trainData := [][]int{}
	var counts [10]int
	for sin.Scan() {
		line := sin.Text()
		split := strings.Split(line, ",")
		data := []int{}
		for _, v := range split {
			n, err := strconv.ParseInt(v, 10, 64)
			if err != nil {
				log.Fatal(err)
			}
			data = append(data, int(n))
		}
		counts[data[0]]++
		trainData = append(trainData, data)
	}
	file.Close()
	fmt.Println("train data loaded. rows:", len(trainData))

	// test
	file, err = os.Open(fileNameTest)
	if err != nil {
		log.Fatal(err)
	}
	sin = bufio.NewScanner(file)
	nRows := 0
	nCorrect := 0
	for sin.Scan() {
		nRows++
		line := sin.Text()
		split := strings.Split(line, ",")
		data := []int{}
		for _, v := range split {
			n, err := strconv.ParseInt(v, 10, 64)
			if err != nil {
				log.Fatal(err)
			}
			data = append(data, int(n))
		}
		// fmt.Println("data:", data)

		// predict
		// calculate each number's errors
		var errors [10]float64 // each number
		for _, d := range trainData {
			error := sumSqErr(data[1:], d[1:])
			errors[d[0]] += float64(error)
		}
		// make mean
		for i := range errors {
			errors[i] /= float64(counts[i])
		}

		best := 0
		minError := -1.0
		for i, d := range errors {
			error := d
			if minError < 0 || error < minError {
				minError = error
				best = i
			}
		}
		if best == int(data[0]) {
			nCorrect++
		}
		fmt.Println("predicted:", best, "answer:", data[0], best == data[0], "accuracy:", float64(nCorrect)/float64(nRows))
	}
	file.Close()
	fmt.Println("test data rows:", nRows, "accuracy:", float64(nCorrect)/float64(nRows))
}
