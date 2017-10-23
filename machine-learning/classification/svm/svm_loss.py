# Loss function
def svm_loss(W, X, y, reg):
    """
    Linear SVM loss function, vectorized implementation.
    """
    loss = 0.0
    dW = np.zeros(W.shape).astype('float') # initialize the gradient as zero
    
    num_train = X.shape[0]
    num_classes = W.shape[1]
       
    scores = X.dot(W)
    correct_class_score = np.diagonal(scores[:, y])
    incorrect_classes = np.ones(scores.shape) * np.arange(0, num_classes) != y[:, np.newaxis]
    margin = np.maximum(0, scores - correct_class_score[:, np.newaxis] + 1)
    loss += np.sum(margin[incorrect_classes])
    loss /= num_train
    loss += reg * np.sum(W * W)
    
    # indicate the +/- of x_i
    base = np.zeros(scores.shape)
    
    raw_margin = scores - correct_class_score[:, np.newaxis] + 1
    incorrect_class_margin = raw_margin[incorrect_classes].reshape((num_train, num_classes - 1))
    
    correct_classes = np.ones(scores.shape) * np.arange(0, num_classes) == y[:, np.newaxis]
    base[correct_classes] = -np.sum(incorrect_class_margin > 0, axis = 1)
    base[incorrect_classes] = (incorrect_class_margin > 0).astype('float').reshape((num_train * (num_classes - 1)))
    
    dW = X.T.dot(base)
    
    dW /= num_train
    dW += reg * 2 * W
    
    return loss, dW