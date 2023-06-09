# DDD and FP Can’t Be Friends, Yet

## Introduction

Open Slides
Show introduction slide
Mike: I'm Mike and hang around FP conferences. I use Emacs.
Henning: Im Henning and hang around DDD and Java, C# and even PHP conferences. I use Vee-Eye. How do you do, Mike?

Mike: I have a customer

Slide "Cart"

Henning: Lets's model Domain Story EAKI-1
Mike: I see cart, cart, cart -> Lets write code to model the cart
Henning: I would do strategic design first

## Strategic Design

Henning: Draw boundaries EAKI-1
[Mike: Want to write code!]

Henning: Lets look at more detail at the bounded context "delivery".
Models AEKI-2 and -3.

... Packstation ...

H&M agree to focus on collection, so we learn live
about problems with delivery address.

## Mike writes code

```haskell
module Cart where

data Article =
    Lifestyle
  | Furniture
  deriving Show

data Address =
    HomeAddress
  | PackStation
  deriving Show

data TentativeShippingAddress
  = NoAddress
  | ValidAddress ShippingAddress
  | InvalidAddress ShippingAddress InvalidAddressReason

data InvalidShippingAddressReason
    = NoFurnitureToPackStation
```

Henning objects: Not discussed with domain expert.

Mike: You write code, Henning.

## OO Modeling

Henning: I would start in a TDD/BDD fashion.

Starts with two first test cases that come directly out of EAKI-2 and -3:

```java
import static org.assertj.core.api.Assertions.*;

import org.junit.jupiter.api.Test;


public class CartTest {
    @Test
    void givenACart_whenPuttingALifestyleArticle_thenDeliveryAddressCanBePackstation() {
        // given
        var cartUnderTest = new Cart();

        // when
        cartUnderTest.put(new Article(new ArticleName("White Candles"), ArticleCategory.LIFESTYLE));

        // then
        assertThat(cartUnderTest.isDeliverableTo(AddressType.HOME_ADDRESS)).isTrue();
    }

    @Test
    void givenAnEmptyCart_whenPuttingAFurnitureArticle_thenDeliveryAddressCannotBePackstation() {
        // given
        var cartUnderTest = new Cart();

        // when
        cartUnderTest.put(new Article(new ArticleName("Leather sofa"), ArticleCategory.FURNITURE));

        // then
        assertThat(cartUnderTest.isDeliverableTo(AddressType.PACKSTATION)).isFalse();
    }
}
```

Henning then uses the IDE to generate code in the following order:

- empty class Cart -> `@Entity`
- empty class Article -> another Entity
- record ArticleName with a field "name"-> a `@ValueObject`
- enum ArticleCategory -> @VO
- Constant LIFESTYLE
- class Article -> another Entity with a generated constructor
- method `Cart::put()`
- enum AddressType
- constant `HOME_ADDRESS`

Mike interrupts: OK, Henning, you design behavior - method signatures.  I design data.  Let me show how these two go together.

```haskell
checkShippingAddress :: Address -> Article -> TentativeAddress
checkShippingAddress PackStation Furniture =
  InvalidShippingAddress PackStation NoFurnitureToPackStation
checkShippingAddress address article =
  ShippingAddress address
```

Conclusion: Different approaches, but we can benefit from
each other.

## Shopping cart

Mike: Henning, how would you flesh out the `Cart` class?

... TODO Henning ...

... validation? ...

```haskell
data Cart
    = ReadyCart [Article] Address
    | DraftCart [Article] TentativeAddress
```

Henning: You just made up new words.

H&M: But “ready” and “draft” are not an immediate part of the 
domain concept, and thus the domain vocabulary.  Need to
develop new words.

```haskell
data Cart
    = GreenCart [Article] Address
    | YellowCart [Article] TentativeAddress
```

Henning: Again, Mike, we have to check these words wiht our domain experts. Dear auditorium, you buy furniture yourself. Quick raise of hands—who is for solution a, who for solution b?


... validation ...


## Abstraction (optional)

```haskell
data Tentative reason a
  = TentativeGood a
  | TentativeInvalid a [reason]

instance Functor (Tentative reason) where ...

instance Applicative (Tentative reason) where ...
```

Henning: OMG, next thing your’re gonna write is a monad...

Mike: Great idea!

```haskell
instance Monad (Tentative reason) where ...
```

Henning: WHY? Those are all great FP terms, but they are not part of the domain vocabulary and totally disconnected to the domain. This is all about wanking/jerking of on FP terms/math but not about shopping!

Mike: They're not part of the domain vocabulary, they are
*properties* of things in your domain vocabulary.  I'm using
math to learn about the domain.  

Henning: Hold your horses, now it's 4:40 pm on a Friday after a long conference. I think we want come from the code to a more abstract level. What now?

Mike: We can try to distill what we just did into a couple of fundamentals.

Slides (UL vs. Abstraction)


## Optional

Also, they are useful
when writing code:

```haskell
type TentativeAddress
  = Tentative InvalidShippingAddressReason Address

data Cart
  = GreenCart [Article] Address
  | YellowCart [Article] (Maybe TentativeAddress)

putArticleInCart article (YellowCart articles tentativeAddress) =
  YellowCart
    (articles ++ [article])
    (fmap (>>= checkShippingAddress article) tentativeAddress)

 -- cart :: [Article] -> TentativeAddress -> Cart
cart :: [Article] -> Maybe (Tentative InvalidShippingAddressReason Address) -> Cart
cart articles maybeTentativeAddress =
  case fmap (fmap (GreenCart articles)) maybeTentativeAddress of
    Just (TentativeGood cart) -> cart
    _ -> YellowCart articles maybeTentativeAddress
```

- article -> entity -> ID
