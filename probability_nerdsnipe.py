import random

"""
Logic:

1. We want a uniform random integer in {1, ..., 7}, but we only have a
   generator (fxn_5) for {1, ..., 5}.

2. Two independent calls to fxn_5 yield a pair (X1, X2), each
   uniformly from {1, ..., 5}. Subtract 1 from each to get X = X1 - 1
   and Y = X2 - 1, which are uniformly distributed in {0, 1, 2, 3, 4}.

3. Combine these two numbers into a single number using: z = 5 * X + Y
   Since X and Y are each in {0,...,4}, z is uniformly distributed in
   {0, 1, 2, ..., 24} (there are 5*5 = 25 outcomes). You can think of
   this as translating our pairs (X1, X2) into a single number z in
   base-5.

4. We only accept outcomes where z is in {0, ..., 20} (21 outcomes)
   because 21 is divisible by 7. Map the accepted outcomes to {1, ...,
   7} using: result = (z % 7) + 1

5. If z is in {21, 22, 23, 24} (4 outcomes), we reject and try again.

6. The acceptance probability per attempt is 21/25. On average, it
   takes 1/(21/25) = 25/21 attempts to get a valid result.  Since each
   attempt uses 2 calls to fxn_5, the average number of calls is: 2 *
   (25/21) = 50/21 or approximately 2.38.

Notes:

Rejection sampling is a technique where we sample from an
easy-to-sample proposal distribution and then accept or reject the
sample based on the ratio of the target density to the proposal
density.

This basic idea is key to Bayesian model fitting. In Bayesian
inference, our target is often a (very) complex posterior
distribution, which is hard to sample from directly as it is usually
some very high dimensional chain of gnarly integrals.

Rejection sampling lays the foundation for advanced methods like MCMC
(Markov Chain Monte Carlo), where proposals are accepted or rejected
in a way that eventually produces samples from the target
distribution.

Extensions such as Hamiltonian Monte Carlo and Adaptive Rejection
Sampling improve efficiency by using gradient information or by
adapting the proposal dynamically.
"""

def fxn_5():
    """Return a random integer uniformly from 1 to 5"""
    return random.randint(1, 5)

def fxn_7_with_count():
    """
    Generate a random integer uniformly from 1 to 7 using rejection sampling with fxn_5

    Each attempt uses 2 calls to fxn_5

    Returns:
      - A uniformly generated integer in {1, 2, ..., 7}
      - The number of calls to fxn_5 used for this sample
    """
    calls = 0
    while True:
        # Each attempt uses 2 calls.
        calls += 2

        # Call fxn_5 twice and shift the output to get values in {0, 1, 2, 3, 4}.
        x = fxn_5() - 1  # Now x is in {0, 1, 2, 3, 4}.
        y = fxn_5() - 1  # Now y is in {0, 1, 2, 3, 4}.

        # Combine the two values into a single number z in {0, ..., 24} using base-5 arithmetic.
        # In any base b numeral system, a two-digit number with digits d1 and d2 is calculated as:
        # b * d1 + d2
        z = 5 * x + y

        # Accept z if it is in the range 0 to 20, otherwise reject
        if z < 21:
            return calls


# Simulation: generate samples to compute the average number of fxn_5 calls
num_samples = 1_000_000
total_calls = 0

for _ in range(num_samples):
    calls = fxn_7_with_count()
    total_calls += calls

average_calls = total_calls / num_samples
print(f"Average number of fxn_5 calls per sample: {average_calls}")
