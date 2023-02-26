# Revision history for NaiveBayes

## 0.1.0.0 -- 2023-02-22
Set-up

## 0.1.0.1 -- 2023-02-23
Added ability to read the text (String) in txt file inside a directory, and do this for every single txt file.
The result can be stored in a <String> array 

## 1.1.0.0 -- 2023-02-26
Added some CLI functionality, a lot of functions are moved around. 
Most of the previous usage for the program has been changed hence the version update.

TODO for next version:
1) Perform validation on dataset
2) Add ability to train a model using random review data (25-75, 50-50, 25-75 %train-test splits)
    - e.g. come up with a way to use random number generator to create an array of randomly picked files to use for training.
    - the test files will just be the left over files.
    - maybe need to encorporate it into some data structure to capture the state of the CLI program, because
    - the idea is that the user can train and test at a later time after several load (or save, train..) operations 
    - (but perhaps this is too much and maybe just better to make validation run always after training?)
3) Add some way for the user to input their own text into the console and have the currently selected trained model give a verdict if their sentence is positive or negative (with  some certainty score or verification score) 