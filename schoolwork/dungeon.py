# Author: L Denney
# 261042191

ROOM_NAME = "Sea Bed"
AUTHOR = "L"
PUBLIC = True

def escape_room():
    ''' () -> NoneType
    Prints opening dialogue and asks for user input.
    Calls whichever function is relevant to the user's input.
    While an invalid command was entered or "list commands" was entered,
    calls invalid_command() or  list_commands() respectively.
    
    >>>escape_room()
    You wake up on the sandy ocean floor, eels and colourful fish dart in and out of the tall coral structures that surrounds you.
    You try to stand up but your legs are tied with rope. A crab skuttles by.
    What would you like to do?
        >talk to crab
   
    >>>escape_room()
    You wake up on the sandy ocean floor, eels and colourful fish dart in and out of the tall coral structures that surrounds you.
    You try to stand up but your legs are tied with rope. A crab skuttles by.
    What would you like to do?
        > nothing
    
    Invalid command entered
    Enter "List commands" for available options
    What would you like to do?
        > inspect sand
    '''
    
    print("You wake up on the sandy ocean floor, eels and colourful fish dart in and out of the tall coral structures that surrounds you.")
    print("You try to stand up but your legs are tied with rope. A crab skuttles by.")
    
    action = input("What would you like to do? ")
    action = action.lower()
    
    while not sand_string(action) and not crab_string(action):
        if action == "list commands":
            list_commands("Talk to crab", "Dig in sand")
            action = input("What would you like to do? ")
            action = action.lower()
        
        else:
            action = invalid_command()
        
    if crab_string(action):
        talk_to_crab()
        
    elif sand_string(action):
        inspect_sand()
        
        
def list_commands(command1, command2):
    """ (str,str) -> NoneType
    Takes two strings of whichever commands are relevant to the
    function that called this one
    Prints given commands.
    
    >>> list_commands("talk to crab", " ")
    talk to crab
    
    >>>list_commands("jump", "100")
    100
    jump

    >>>list_commands(" ", " ")
    
    
    
    """
    
    if command2 == " ":
        print(command1)
        
    else:
        print(command2)
        print(command1)
        

def invalid_command():
    """ () -> str
    Prints "Invalid command entered" and "Enter "List commands" for available options"
    Asks user for another input.
    Returns string of user input in lowercase.
    
    >>>invalid_command()
    Invalid command entered
    Enter "List commands" for available options
    What would you like to do?
        > jump

    'jump'

    >>>invalid_command()
    Invalid command entered
    Enter "List commands" for available options
    What would you like to do?
        >list commands
    
    'list commands'
    
    >>>invalid_command()
    Invalid command entered
    Enter "List commands" for available options
    What would you like to do?
        > nothing
    
    'nothing'
    
    """
    
    print ("Invalid command entered")
    print("Enter \"List commands\" for available options")
    action = input("What would you like to do? ")
    action = action.lower()
     
    return action



        
def talk_to_crab():
    """ () -> NoneType
    Prints some narration,
    Asks user to input another action.
    While an invalid command was entered or "list commands" was entered,
    calls invalid_command() or  list_commands() respectively.
    When special command is entered, prints escape message and Returns.
    
    >>>talk_to_crab()
    You ask the crab for help. He cuts the rope that is binding your legs with his pinchers.
    What would you like to do?
    >swim up
    
    >>>talk_to_crab()
    You ask the crab for help. He cuts the rope that is binding your legs with his pinchers.
    What would you like to do?
        >list commands
    swim up
    What would you like to do?
        >talk to crab
    Invalid command entered
    Enter "List commands" for available options
    What would you like to do?
        >swim up
    You have escaped the sea bed.
    """
    
    print("You ask the crab for help. He cuts the rope that is binding your legs with his pinchers.")
    
    action = input("What would you like to do? ")
    action = action.lower()
    
    
    while not escape_string(action):
        if action == "list commands":
            list_commands("swim up", " ")
            action = input("What would you like to do? ")
            action = action.lower()
            
        else:   
            action = invalid_command()

            
    print("You have escaped the sea bed.")
    return
    
def inspect_sand():
    """ () -> NoneType
    Prints some narration,
    While an invalid command was entered or "list commands" was entered,
    calls invalid_command() or  list_commands() respectively.
    Calls appropriate function from user's input.
    
    >>> inspect_sand()
    You run your hands through the sand. It is dry. You feel something cold. Fish swarm to see what you've found. It's an old whistle
    What would you like to do?
        >blow whistle
    
    >>> inspect_sand()
    You run your hands through the sand. It is dry. You feel something cold. Fish swarm to see what you've found. It's an old whistle
    What would you like to do?
        >4
    Invalid command entered
    Enter "List commands" for available options
    What would you like to do?
        >BloW WhiStle
    
    """
    
    print("You run your hands through the sand. It is dry. You feel something cold. Fish swarm to see what you've found. It's an old whistle")
    
    action = input("What would you like to do? ")
    action = action.lower()
    
    while not fish_string(action) and not whistle_string(action):
        if action == "list commands":
            list_commands("Talk to the fish", "Blow whistle")
            action = input("What would you like to do? ")
            action = action.lower()
            
        else:
            action = invalid_command()
        
    
    if fish_string(action):
        talk_to_fish()
    if whistle_string(action):
        blow_whistle()

def blow_whistle():
    """ () -> NoneType
    Prints some narration,
    While an invalid command was entered or "list commands" was entered,
    calls invalid_command() or  list_commands() respectively.
    Calls talk_to_crab() when user's input evaluates to True in crab_string().
    
    >>>blow_whistle()
    You blow the whistle. Out of the coral, a crab emerges and skuttles over to you.
    What would you like to do?
        >talk to crab
        
    >>>blow_whistle()
    You blow the whistle. Out of the coral, a crab emerges and skuttles over to you.
    What would you like to do?
        >swim up
    Invalid command entered
    Enter "List commands" for available options
    What would you like to do?
        >talk to CraB
        
    >>>blow_whistle()
    You blow the whistle. Out of the coral, a crab emerges and skuttles over to you.
    What would you like to do?
        >LiST Commands
    Talk to crab
    What would you like to do?
        >TALK TO CRAB
    
    """
    
    print("You blow the whistle. Out of the coral, a crab emerges and skuttles over to you.")
    
    action = input("What would you like to do? ")
    action = action.lower()
    
    while not crab_string(action):
        if action == "list commands":
            list_commands("Talk to crab", " ")
            action = input("What would you like to do? ")
            action = action.lower()
            
        else:
            action = invalid_command()

    talk_to_crab()
    
        
def talk_to_fish():
    ''' () -> NoneType
    Prints some narration,
    While an invalid command was entered or "list commands" was entered,
    calls invalid_command() or  list_commands() respectively.
    Calls blow_whistle() when user's input evaluates to True in whistle_string().
    
    >>>talk_to_fish()
    You ask the fish for advice. They tell you to blow the whistle
    What would you like to do?
        >blow Whistle
        
    >>>talk_to_fish()
    You ask the fish for advice. They tell you to blow the whistle
    What would you like to do?
        >ask fish for help
    Invalid command entered
    Enter "List commands" for available options
    What would you like to do?
        >talk to the crab
    Invalid command entered
    Enter "List commands" for available options
    What would you like to do?
        >blow whistle
        
    >>>talk_to_fish()
    You ask the fish for advice. They tell you to blow the whistle
    What would you like to do?
        >use WHISTle
    
    '''
    
    print("You ask the fish for advice. They tell you to blow the whistle")
    
    action = input("What would you like to do? ")
    action = action.lower()
        
    while not whistle_string(action):
        if action == "list commands":
            list_commands("Blow whistle", " ")
            action = input("What would you like to do? ")
            
        else:
            action = invalid_command()
        
    blow_whistle()
    
    
def sand_string(dig_in_sand):
    ''' (str) -> bool
    Takes a lowercase string of user's input and compares it to multiple similar commands.
    If there is a match, Returns True.
    
    >>>sand_string("dig in sand")
    True
    
    >>>sand_string("dig in the sand")
    True
    
    >>>sand_string("sand")
    False
    
    '''
    
    if dig_in_sand == "dig in sand" or dig_in_sand == "dig in the sand" or dig_in_sand == "inspect sand":
        return True
    
    return False

def crab_string(talk_crab):
    ''' (str) -> bool
    Takes a lowercase string of user's input and compares it to multiple similar commands.
    If there is a match, Returns True.
    
    >>>crab_string("ask crab for help")
    True
    
    >>>crab_string("Ask crab for Help")
    False
    
    >>>crab_string("crab")
    False
    
    '''
    
    if talk_crab == "talk to crab" or talk_crab == "ask crab for help" or talk_crab == "talk to the crab":
        return True
    
    return False
    
def whistle_string(use_whistle):
    ''' (str) -> bool
    Takes a lowercase string of user's input and compares it to multiple similar commands.
    If there is a match, Returns True.
    
    >>>whistle_string("use_whistle")
    False
    
    >>>whistle_string("blow whistle")
    True
    
    >>>whistle_string("Use Whistle")
    False
    '''
    
    if use_whistle == "blow whistle" or use_whistle == "use whistle":
        return True
    
    return False
    
def fish_string(fish_help):
    """ (str) -> bool
    Takes a lowercase string of user's input and compares it to multiple similar commands.
    If there is a match, Returns True.
    
    >>>fish_string("fish_help")
    False
    
    >>>fish_string("talk to fish")
    True
    
    >>>fish_string("ask fish for help")
    True
    """
    if fish_help == "talk to the fish" or fish_help == "talk to fish" or fish_help == "ask fish for help":
        return True
    
    return False
    
def escape_string(escape):
    ''' (str) -> bool
    Takes a lowercase string of user's input and compares it to multiple similar commands.
    If there is a match, Returns True.
    
    >>>escape_string("escape")
    True
    
    >>>escape_string("swim away")
    True
    
    >>>escape_string("Escape")
    False
    '''
    
    if escape == "swim up" or escape == "swim away" or escape == "escape":
        return True
    
    return False


    

