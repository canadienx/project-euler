### Project Euler - Ruby

module Euler
  require "eulerutils"

  ## Problem 1
  def Euler.euler1(limit=1000)
    sum = 0
    limit.times do |x|
      if x % 5 == 0 || x % 3 == 0
        sum += x
      end
    end
    
    return sum
  end

  ## Problem 2
  def Euler.euler2(limit=4000000)
    sum = 0
    Math.fibo_upto(limit) do |f|
      sum += f if f.even?
    end
    return sum
  end

  ## Problem 3
  def Euler.euler3(x=600851475143)
    x.factors.select {|f| f.prime?}.max
  end

  ## Problem 4
  def Euler.euler4()
    Euler.all_products(100, 999).select {|x| x.palindrome?}.max
  end

  def Euler.all_products(start, stop)
    (start..stop).collect {|x| Euler.product_list(start, x, x) }.flatten
  end
  
  def Euler.product_list(start, stop, x)
    (start..stop).collect {|y| x * y}
  end

  ## Problem 5
  def Euler.euler5(start=1, stop=20)
    (start..stop).reduce(:lcm)
  end

  ## Problem 6
  def Euler.euler6(limit=100)
    (1..limit).reduce(:+).square -
      (1..limit).collect { |x| x.square }.reduce(:+)
  end

  ## Problem 7
  def Euler.euler7(n=10001)
    Math.nth_prime(n)
  end

  ## Problem 8
  EULER8 = 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450
  def Euler.euler8(seq_size=5, number=EULER8)
    digits = number.digits
    cur_prod = digits[0..seq_size].reduce(:*)
    max_prod = cur_prod
    zero_range = false

    digits[seq_size..digits.length].each_with_index do |d, i|
      i += 5
      if digits[i - seq_size + 1..i].include?(0)
        zero_range = true;
      else
        if zero_range
          cur_prod = digits[i - seq_size + 1..i].reduce(:*)
          zero_range = false
        else
          cur_prod = cur_prod / digits[i - seq_size] * d
        end
      end
      max_prod = cur_prod if cur_prod > max_prod
    end

    return max_prod
  end

  ## Problem 9
  def Euler.euler9(sum=1000)
    sum.times do |x|
      sum.times do |y|
        if Euler.pythagorean_triple?(x, y, 1000 - (x + y))
          return x * y * (1000 - (x + y)) 
        end
      end
    end
  end

  def Euler.pythagorean_triple?(x, y, z)
    c = [x, y, z].max
    ab = [x, y, z].reject { |x| x == c }
    ab.length == 2 and ab[0].square + ab[1].square == c.square
  end


  ## Problem 10
  def Euler.euler10(limit=2000000)
    sum = 0
    (2..limit).each do |x|
      sum += x if x.prime?
    end
    return sum
  end
end

