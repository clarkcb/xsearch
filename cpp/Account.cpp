#include "Account.h"

Account::Account(double b)
{
    balance = b;
}

void Account::deposit(double amt)
{
    balance += amt;
}

void Account::withdraw(double amt)
{
    balance -= amt;
}

double Account::getBalance() const
{
    return balance;
}