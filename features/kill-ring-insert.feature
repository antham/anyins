Feature: Insert content on several lines from kill-ring

  Scenario: Copy a region and insert it from place
    Given I switch to buffer "list-of-things"
    Given I clear the buffer
    Given the following text in buffer:
    """
    a fruit
    a fruit
    a fruit
    a fruit
    a fruit
    a fruit
    """
    Given the following text in kill-ring:
    """
     could be very good
     could be red and tasty
     could be spiky
     could be yellow
     could be tiny
     could be round and orange
    """
    When I place the cursor after "a fruit"
    Given I start an action chain
    And I press "M-x"
    And I type "anyins-mode"
    And I press "RET"
    And I press "k"
    And I execute the action chain
    Then I should see in buffer
    """
    a fruit could be very good
    a fruit could be red and tasty
    a fruit could be spiky
    a fruit could be yellow
    a fruit could be tiny
    a fruit could be round and orange
    """

  Scenario: Copy a region and insert it from place at the end of
    irregular lines
    Given I switch to buffer "list-of-things"
    Given I clear the buffer
    Given the following text in buffer:
    """
    category
    name
    color
    weight
    """
    Given the following text in kill-ring:
    """
     : fruit
     : strawberry
     : red
     : 8
    """
    When I place the cursor after "category"
    Given I start an action chain
    And I press "M-x"
    And I type "anyins-mode"
    And I press "RET"
    And I press "k"
    And I execute the action chain
    Then I should see in buffer
    """
    category : fruit
    name     : strawberry
    color    : red
    weight   : 8
    """

  Scenario: Copy a region and insert it where marks are defined
    Given I switch to buffer "list-of-things"
    Given I clear the buffer
    Given the following text in buffer:
    """
    apple is a fruit
    carrot is a vegetable
    strawberry is a fruit
    cauliflower is a vegetable
    pineapple is a fruit
    """
    Given the following text in kill-ring:
    """
     very good
     red and tasty
     spiky
    """
    When I place the cursor after "apple is a"
    Given I start an action chain
    And I press "M-x"
    And I type "anyins-mode"
    And I execute the action chain
    Given I start an action chain
    And I press "RET"
    And I execute the action chain
    When I place the cursor after "strawberry is a"
    Given I start an action chain
    And I press "RET"
    And I execute the action chain
    When I place the cursor after "pineapple is a"
    Given I start an action chain
    And I press "RET"
    And I execute the action chain
    Given I start an action chain
    And I press "k"
    And I execute the action chain
    Then I should see in buffer
    """
    apple is a very good fruit
    carrot is a vegetable
    strawberry is a red and tasty fruit
    cauliflower is a vegetable
    pineapple is a spiky fruit
    """
