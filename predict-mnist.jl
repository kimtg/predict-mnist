using DelimitedFiles
using Random

square(x) = x * x

function sum_sq_err(xs::Vector, ys::Vector, start::Int)
    s = 0
    for i in start:length(xs)
        s += square(xs[i] - ys[i])
    end
    return s
end

function predict(train_data::Vector{Vector{Float64}}, test::Vector{Float64})
    predicted = 0.0
    min_error = -1.0    
    for train in train_data
        error = sum_sq_err(test, train, 1)
        if error < min_error || min_error < 0
            predicted = train[1]
            min_error = error
        end
    end
    return predicted
end

function main()
    file_name_train = "mnist_train.csv"
    file_name_test = "mnist_test.csv"

    # train
    train_data = open(file_name_train) do io
        lines = readlines(io)
        [parse.(Float64, split(line, ",")) for line in lines]
    end
    println("train data loaded. rows: $(length(train_data))")

    # test
    n_rows = 0
    n_correct = 0
    open(file_name_test) do io
        for line in eachline(io)
            test = parse.(Float64, split(line, ","))
            n_rows += 1
            predicted = predict(train_data, test)
            answer = test[1]
            if predicted == answer
                n_correct += 1
            end
            accuracy = n_correct / n_rows
            println("row: $n_rows predicted: $predicted answer: $answer accuracy: $accuracy")
        end
    end
end

main()