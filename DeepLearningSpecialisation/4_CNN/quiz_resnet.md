- Question 1
In order to be able to build very deep networks, we usually only use pooling layers to downsize the height/width of the activation volumes while convolutions are used with “valid” padding. Otherwise, we would downsize the input of the model too quickly.
    False

- Question 2
Training a deeper network (for example, adding additional layers to the network) allows the network to fit more complex functions and thus almost always results in lower training error. For this question, assume we’re referring to “plain” networks. 
    False

- Question 3
Which ones of the following statements on Inception Networks are true? (Check all that apply.)
    True: A single inception block allows the network to use a combination of 1x1, 3x3, 5x5 convolutions and pooling.
    False: Making an inception network deeper (by stacking more inception blocks together) should not hurt training set performance.
    True: Inception blocks usually use 1x1 convolutions to reduce the input data volume’s size before applying 3x3 and 5x5 convolutions.
    False: Inception networks incorporates a variety of network architectures (similar to dropout, which randomly chooses a network architecture on each step) and thus has a similar regularizing effect as dropout.
(I think the softmax layer outputs from hidden layers act as regularization units here!)

- Question 4
Which of the following do you typically see as you move to deeper layers in a ConvNet?
    nH and nW decrease, while nC increases

- Question 5 
Which of the following do you typically see in a ConvNet? (Check all that apply.)
    Multiple CONV layers followed by a POOL layer
    FC layers in the last few layers

