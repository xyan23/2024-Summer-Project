// R-1.1
/* Answer: d is not a valid C++ variable name because it has a special character 
"$". All the other variable names are valid since they all start with either underscore 
or letter, and there is no special character besides underscore. */

// R-1.2

/* Pseudo-code
findS_L(A)
    x = A[0]
    smallest = x
    largest = x
    for i = A.size downto 1
        if A[i] < smallest
            smallest = A[i]
        if A[i] > largest
            largest = A[i]
    return smallest, largest
*/

void findS_L(int a[]){
    int n = sizeof(a);
    smallest = a[0];
    largest = a[0];
    for (int i = 1; i < n; i++) {
        if (a[i] < smallest)
            smallest = a[i];
        else if (a[i] > largest)
            largest = a[i];
    }
    return smallest;
    return largest;
}

// R-1.3
struct Pair {
    int first;
    double second;
};

// R-1.4
// Answer: abcabcdabc

// R-1.5
// Answer: (y + (2 * (z++))) < (3 - (w / 5))

// R-1.6
double* dp[10];
for (int i = 0; i < 10, i++){
    dp[i] = new double;
    *dp[i] = 0.0;
}

// R-1.7
int sumInt(int n) {
    int sum = 0;
    for (int i = 0; i <= n; i++) {
        sum += i;
    }
    return sum;
}

// R-1.8
bool isMultiple(long n, long m){
    if (n <= 0 || m <= 0) {
        return false;
    }

    return (n % m == 0);
}

// R-1.9
#include <iostream>
using namespace std;

void printArray(int** A, int m, int n){
    for (int i = 0; i < m; ++i) {
        for (int j = 0; j < n; ++j){
            cout << A[i][j] << endl;
        }
    }
}

// R-1.10
/* Function f passing the argument by value, which is the system
makes a copy of the variable to be passed to the function.
Function g passing the argument by reference, which is any modifica
-tions made to an argument in the function modifies the corresponding 
actual argument. */

// R-1.11
#include <string>
class Flower {
public:
    //constructor
    Flower(const std::string& nm, int no, float price=0);
    //accessor functions
    std::string getName() const {return name;}
    int getNumber() const {return number;}
    float getPrice() const {return price;}

private:
    std::string name; // name of the flower
    int number; // number of pedals
    float price; // price
}

// R-1.12
bool CreditCard::chargeIt(double price) {
    if ((price + balance) > double(limit) || price <= 0)
        return false;
    balance += price;
    return true;
}

void CreditCard::makePayment(double payment) {
    if (payment > 0)
        balance -= payment;
}

// R-1.13, R-1.14
#ifndef CREDIT_CARD_H
#define CREDIT_CARD_H

#include <string>
#include <iostream>

class CreditCard {
public:
    // Constructor
    CreditCard(const std::string& no, const std::string& nm, int lim, double bal=0);

    // Accessor functions
    std::string getNumber() const { return number; }
    std::string getName() const { return name; }
    double getBalance() const { return balance; }
    int getLimit() const { return limit; }

    bool chargeIt(double price); // Make a charge
    void makePayment(double payment); // Make a payment
    void chargeInterest(double interest); // Charge interest
    void lateFee(double latefee); // Charge late fee

private:
    std::string number; // Credit card number
    std::string name;   // Card owner's name
    int limit;          // Credit limit
    double balance;     // Credit card balance
};

// Stream insertion operator to print card information
std::ostream& operator<<(std::ostream& out, const CreditCard& c);

#endif 


// R-1.15
// Modifer functions
std::string newNumber(std::string newnumber) // Modify card number
std::string newName(std::string newname) // Modify card holder's name
double newBalance(double newbalance) // Modify card balance
int newLimit(int newlimit) // Modify card limit

// R-1.16
for (int j = 1; j <= 16; ++j) {
        wallet[0]->chargeIt(double(j)); // Make the charge high enough to breach the limit
        wallet[1]->chargeIt(double(26 * j));
        wallet[2]->chargeIt(double(3 * j));
    }

/* Wallet 2 will go over its credit limit with balance 3536, which is bigger than the
3500 limit. The other wallets does not go over the limit. 
*/

// R-1.17
class AllKinds {
public:
    //constructor
    AllKinds(int k = 5, float m = 10.8, long n = 1000);
    //accessor functions
    int getInt() const {return Int;}
    float getFloat() const {return Float;}
    long getLong() const {return Long;}

    float getIF() const {return Int + Float;}
    float getFL() const {return Float + Long;}
    long getIL() const {return Int + Long;}
    float getIF() const {return Int + Float + Long;}

private:
    int Int; 
    float Float; 
    long Long;
}

// R-1.18
bool isMultiple(long n, long m){
    return (n % m == 0);
}

// R-1.19
#include <cmath>

bool isTwoPower(int i){
    float f = log2(i);
    if (i < 0)
        return false;
    else
        if (ceil(f) == floor(f))
            return true;
        else
            return false;
}

// R-1.20
int sumInt(int n) {
    int sum = 0;
    for (int i = 0; i <= n; i++) {
        sum += i;
    }
    return sum;
} 

// R-1.21
int sumOddInt(int n) {
    int sum = 0;
    for (int i = 0; i <= n; i++) {
        if (i % 2 != 0)
            sum += i;
    }
    return sum;
} 

// R-1.22
int timesDivideByTwo(double x){
  int count = 0;
  if (x <= 0)
    return false;
  else
    while (x >= 2){
      x = x / 2;
      count += 1;
    }
  
  return count;
}