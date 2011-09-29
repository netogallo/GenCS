#Set counter for GenCS Jacobs 2011
#By Ernesto Rodriguez and Naomi Pentrel


#c(n=1)=2
#c(n>1)=(c(n-1)*3)-1

S=Set([1,2,3,4,5])

#Solution for part a of problem 2.5
def megaSet(S):
    P=[]
    for x in S.subsets():
        for y in S.subsets():
            if x.union(y)==S:
                if(not ((x,y) in P)) and (not (y,x) in P):
                    P.append((x,y))
                    print (x,y)

    return P


def megaSet2(S):
    P=[]
    for x in S.subsets():
        for y in S.subsets():
            if x.union(y)==S:
                if(not ((x,y) in P)):
                    P.append((x,y))
                    print (x,y)

    return P
