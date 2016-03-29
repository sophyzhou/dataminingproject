# import all the related fields 
import pandas as pd
import numpy as np
from pandas import DataFrame

population_a = 'ss13pusa.csv'
population_b = 'ss13pusb.csv'

popa = pd.read_csv(population_a,usecols=['ST', 'PINCP', 'SCHL', 'AGEP', 'ESR', 'SEX', 'NATIVITY'])
popb = pd.read_csv(population_b,usecols=['ST', 'PINCP', 'SCHL', 'AGEP', 'ESR', 'SEX', 'NATIVITY'])
pop_subset = DataFrame(pd.concat([popa,popb],axis = 0))

# get the np.array format of fields 
st = np.array(pop_subset.ST.tolist())
pincp = np.array(pop_subset.PINCP.tolist())
schl = np.array(pop_subset.SCHL.tolist())
agep = np.array(pop_subset.AGEP.tolist())
esr = np.array(pop_subset.ESR.tolist())
sex = np.array(pop_subset.SEX.tolist())
# cow = np.array(pop_subset.COW.tolist()) #too many NaNs
# race = np.array(pop_subset.RAC1P.tolist())
nativity = np.array(pop_subset.NATIVITY.tolist())


from collections import defaultdict
avg_incomes = defaultdict(list)
for idx in xrange(len(pincp)):
    if not np.isnan(pincp[idx]) and pincp[idx] > 0:
        avg_incomes[st[idx]].append(pincp[idx])
for key,val in avg_incomes.iteritems():
    avg_incomes[key] = sum(val) * 1. / len(val)
avg_ = sorted(avg_incomes.items(), key=lambda d: d[1])
low = avg_[:len(avg_)/3]
middle = avg_[len(avg_)/3:len(avg_)*2/3]
high = avg_[len(avg_)*2/3:]


s_label = {}
for s in low:
    s_label[s[0]] = 1
for s in middle:
    s_label[s[0]] = 2
for s in high:
    s_label[s[0]] = 3

for idx,s in enumerate(st):
    st[idx] = s_label[s]

# get the label for classification according to income(pincp)
# (0, 30000): 1(low), [30000, 85000): 2(middle), [85000, ): 3(high)
# the criteria of choosing the two level points: 30000 is the median, 85000 is the 75%th number
pincp_label = []
low = 30000.
middle = 85000.0
middle = np.percentile(pincp, 80)
for income in pincp:
    if income > 0 and income < low:
        pincp_label.append(1)
    elif income >= low and income < middle:
        pincp_label.append(2)
    elif income >= middle:
        pincp_label.append(3)
    elif income == 0 or np.isnan(income):
        pincp_label.append(0)
    else:
        pincp_label.append(1)
#         assert 0 == 1
pincp_label = np.array(pincp_label)


# process the degree, change nan to 0
schl_ = []
for degree in schl:
    if np.isnan(degree):
        schl_.append(int(0))
    else:
        schl_.append(int(degree))
schl_ = np.array(schl_)


# process ages, map age in [x*5, x*5+5) to x*5+5
agep_ = []
for age in agep:
    agep_.append((age+5)//5*5)
agep_ = np.array(agep_)


# process esr, change nan to 0
esr_ = []
for e in esr:
    if np.isnan(e):
        esr_.append(0)
    else:
        esr_.append(int(e))
esr_ = np.array(esr_)

# check lengths of all fields, ignore records with any filed of zero, combine fields into training data
data = []
label = []
count = 0
assert len(st) == len(schl_) == len(agep_) == len(esr_) == len(sex) == len(pincp_label)
for idx in xrange(len(st)):
    if all((st[idx] != 0, schl_[idx] != 0, agep_[idx] != 0, esr_[idx] != 0, sex[idx] != 0, pincp_label[idx] != 0)):
        data.append([nativity[idx], st[idx], schl_[idx], agep_[idx], esr_[idx], sex[idx] ])
        label.append(pincp_label[idx])
data = np.array(data)
label = np.array(label)

# introduce randomness to record choosing
import random
rg = range(len(label))
random.shuffle(rg)
train_size = int(len(label) * 2. / 3.)
# test_size = len(label) - train_size

train_X = data[rg[:train_size]]
train_y = label[rg[:train_size]]
test_X = data[rg[train_size:]]
test_y = label[rg[train_size:]]


# training with svm
from sklearn import svm, cross_validation
limit = len(train_X) / 100
clf = svm.SVC()
scores = cross_validation.cross_val_score(clf, train_X[:limit], train_y[:limit], scoring='accuracy', cv=5, n_jobs = -1) 
print scores
print np.mean(scores)


# decision tree
from sklearn import tree, cross_validation
clf = tree.DecisionTreeClassifier()
limit = len(train_X) 
scores = cross_validation.cross_val_score(clf, train_X[:limit], train_y[:limit], scoring='accuracy', cv=5, n_jobs = -1)
print scores
print np.mean(scores)


from sklearn.linear_model import SGDClassifier
lr = SGDClassifier(loss='log', penalty='l2', shuffle=True)
scores = cross_validation.cross_val_score(lr, train_X[:limit], train_y[:limit], scoring='accuracy', cv=5, n_jobs = -2) 
print scores
print np.mean(scores)


from sklearn.naive_bayes import GaussianNB
limit1 = len(train_y)
limit2 = len(test_y)
gnb = GaussianNB()
p_label = gnb.fit(train_X[:limit1], train_y[:limit1]).predict(test_X[:limit2])
sum(p_label == test_y[:limit2]) / float(limit2)



