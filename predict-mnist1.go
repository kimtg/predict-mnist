// correlation

package main

import (
	"fmt"
	"bufio"
	"strings"
	"os"
	"log"
	"strconv"
	"math"
)

func square(x float64) float64 {
	return x * x
}

func correl(x, y []float64) float64 {
	sumX := 0.0
	sumY := 0.0
	l := len(x)
	if l != len(y) {
		log.Fatal("not same size")
	}
	for i := range x {
		sumX += x[i]
		sumY += y[i]
	}
	meanX := sumX / float64(l)
	meanY := sumY / float64(l)
	varX := 0.0
	varY := 0.0
	sumProdDiff := 0.0
	for i := range x {
		sumProdDiff += (x[i] - meanX) * (y[i] - meanY)
		varX += square(x[i] - meanX)
		varY += square(y[i] - meanY)
	}
	stdevX := math.Pow(varX, 0.5)
	stdevY := math.Pow(varY, 0.5)
	return sumProdDiff / stdevX / stdevY
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
	trainData := [][]float64{}
	for sin.Scan() {
		line := sin.Text()
		split := strings.Split(line, ",")
		data := []float64{}
		for _, v := range split {
			n, err := strconv.ParseFloat(v, 64)
			if err != nil {
				log.Fatal(err)
			}
			data = append(data, n)
		}
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
		data := []float64{}
		for _, v := range split {
			n, err := strconv.ParseFloat(v, 64)
			if err != nil {
				log.Fatal(err)
			}
			data = append(data, n)
		}
		// fmt.Println("data:", data)

		// predict
		best := 0
		maxCorrel := -math.MaxFloat64
		for i, d := range trainData {
			corr := correl(data[1:], d[1:])
			if corr > maxCorrel {
				maxCorrel = corr
				best = int(trainData[i][0])
			}
		}
		if best == int(data[0]) {
			nCorrect++
		}
		fmt.Println("predicted:", best, "answer:", data[0], "accuracy:", float64(nCorrect) / float64(nRows))
	}
	file.Close()
	fmt.Println("train data loaded. rows:", nRows, "accuracy:", float64(nCorrect) / float64(nRows))
}