### Project Euler - Ruby

## Operations on integers
class Integer
  
  def divides?(n)
      (n % self).zero?
  end
  
  def factorial()
    (1..self).reduce(:*)
  end
  
  def gcd(b)
    return self if b.zero?
    return b.gcd(self % b)
  end

  def lcm(b)
    self / self.gcd(b) * b
  end

  def prime?()
    if self == 1
      return false
    end
    if self == 2
      return true
    end
    
    (2..Math.sqrt(self).floor).each do |x|
      return false if x.divides?(self)
    end
    
    return true
  end

  def digits()
    to_s.chars.collect {|c| c.to_i}
  end

  def num_digits()
    (Math.log10(self) + 1).floor
  end

  def factors()
    return [] if self < 1
    return [1] if self == 1

    return (1..Math.sqrt(self).ceil).collect \
    { |x| [x, self / x] if x.divides?(self) }. \
    delete_if { |x| x == nil }.flatten.sort
 
  end

  def proper_factors()
    factors.delete_if { |x| x == self }
  end

  def pandigital?()
    (1..self.digits.length).each do |x|
      if not self.digits.include?(x)
        return false
      end
    end
    return true
  end

  def pandigital_length?(len)
    self.num_digits == len and pandigital?
  end

end


## Operations on any type of number

class Numeric
  require "eulerutils"

  def square()
    self ** 2
  end

  def square?()
    square(Math.sqrt(self).floor()) == self
  end

  def palindrome?()
    self.to_s == self.to_s.reverse
  end

  def power_of(n)
    k = 0
    val = 1
    while val < self
      val = n ** k
      if val == self
        return true
      end
      k += 1
    end
    return false
  end
end

## Operations on strings
class String
  def palindrome?()
    self == self.reverse
  end
end

class Array
  def digits_to_int()
    i = self.length - 1
    total = 0
    self.each do |x|
      total += x * 10 ** i
      i -= 1
    end
    return total
  end
end

## Math operations
module Math
  def Math.logb(num, base)
    log(num) / log(base)
  end

  # Get the nth Fibonacci number
  def Math.fibo(n)
    a1, a2 = 1, 1
    i = 1
    while i <= n
      yield a1
      a1, a2 = a2, a1 + a2
      i += 1
    end
  end

  # Get the Fibonacci numbers up to n
  def Math.fibo_upto(limit)
    a1, a2 = 1, 1
    while a1 <= limit
      yield a1
      a1, a2 = a2, a1 + a2
    end
  end

  # Generate primes numbers up to the nth one
  def Math.generate_primes(n)
    i = 1
    p = 3
    while i <= n
      if i == 1
        yield 2
        i += 1
      elsif i == 2
        yield 3
        i += 1
      else
        p += 2
        if p.prime?
          yield p
          i += 1
        end
      end
    end
  end

      
  # Gets an array of n primes
  def Math.primes_list(n)
    i = 1
    p = 3
    primes = Array.new

    if n >= 1
      primes.push(2)
    end
    
    while i < n
      if p.prime?
        primes.push(p)
        i += 1
      end
      p += 2
    end
    
    return primes
  end

  # Gets the nth prime
  def Math.nth_prime(n)
    i = 2
    p = 3
    
    return 2 if n == 1

    while i < n
      i += 1 if p.prime?
      p += 2
    end

    return p
  end
        
  # Generates primes up to n
  def Math.generate_primes_upto(n)
    (2..n).each do |p|
      yield p if p.prime?
    end
  end
  
  # Get an array of primes up to n
  def Math.primes_upto_list(n)
    (2..n).to_a.delete_if { |m| not m.prime? }
  end

  # Get a hash of primes up to n
  def Math.primes_upto_hash(n)
    res = Hash.new
    (2..n).each do |m|
      if m.prime?
        res[m] = true
      end
    end
    return res
  end

  # Get nth triangle number
  def Math.triangle_number(n)
    n * (n + 1) / 2
  end

  # Get triangle numbers up to n
  def Math.triangle_numbers(n)
    i = 1
    while Math.triangle_number(i) < n
      yield triangle_number(i)
      i += 1
    end
  end

end
