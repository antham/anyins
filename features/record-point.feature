Feature: Record points in buffer

  Scenario: Record points in buffer and abort
    Given I switch to buffer "list-of-things"
    Given I clear the buffer
    Given the following text in buffer:
    """
    A computer is a general purpose device that can
    be programmed to carry out a
    set of arithmetic or logical operations.
    Since a sequence of operations can be readily
    changed, the computer can solve more
    than one kind of problem.
    """
    When I go to beginning of buffer
    When I turn on anyins-mode
    Then mode anyins-mode is enabled
    Then current buffer is read-only
    When I place the cursor before "general"
    Given I start an action chain
    And I press "RET"
    And I execute the action chain
    Then I should have an overlay at point
    When I place the cursor before "arithmetic"
    Given I start an action chain
    And I press "RET"
    And I execute the action chain
    Then I should have an overlay at point
    Then I should have an overlay at point
    When I place the cursor before "sequence"
    Given I start an action chain
    And I press "RET"
    And I execute the action chain
    Then I should have an overlay at point
    When I place the cursor before "kind"
    Given I start an action chain
    And I press "RET"
    And I execute the action chain
    Then I should have an overlay at point
    Given I start an action chain
    And I press "C-g"
    And I execute the action chain
    Then mode anyins-mode is disabled
    Then current buffer is writable
