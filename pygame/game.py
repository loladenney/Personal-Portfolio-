import pygame
from sys import exit
from random import randint, getrandbits, choice

class Player(pygame.sprite.Sprite):
    def __init__(self):
        super().__init__()
        
        player_walk_1 = pygame.image.load('graphics/player/player_walk_1.png').convert_alpha()
        player_walk_2 = pygame.image.load('graphics/player/player_walk_2.png').convert_alpha()
        
        self.player_walk = [player_walk_1,player_walk_2]
        self.player_index = 0
        self.player_jump = pygame.image.load('graphics/player/player_jump.png').convert_alpha()
       
        self.image = pygame.image.load('graphics/player/player_walk_1.png').convert_alpha()
        self.rect = self.image.get_rect(bottomleft = (80,325))
        self.gravity = 0
    
    def player_input(self):
        keys = pygame.key.get_pressed()
        if keys[pygame.K_SPACE] and self.rect.bottom >= 325:
            self.gravity = -20
            
    def apply_gravity(self):
        self.gravity +=1
        self.rect.y += self.gravity
        if self.rect.bottom >= 325:
            self.rect.bottom = 325
            
    def animation_state(self):
        if self.rect.bottom < 325:
            self.image = self.player_jump
        else:
            self.player_index += 0.1
            if self.player_index >= len(self.player_walk):
                self.player_index = 0
            self.image = self.player_walk[int(self.player_index)]
    
    def update(self):
        self.player_input()
        self.apply_gravity()
        self.animation_state()


class Obstacle(pygame.sprite.Sprite):
    def __init__(self, obstacle_type):
        super().__init__()
        
        if obstacle_type == 'fly':
            fly_1 = pygame.image.load('graphics/fly/fly1.png').convert_alpha()
            fly_2 = pygame.image.load('graphics/fly/fly2.png').convert_alpha()
            self.frames = [fly_1,fly_2]
            y_pos =  200
            
        elif obstacle_type == 'snail':
            snail_1 = pygame.image.load('graphics/snail/snail1.png').convert_alpha()
            snail_2 = pygame.image.load('graphics/snail/snail2.png').convert_alpha()
            self.frames = [snail_1, snail_2]
            y_pos =  325
        
        self.animation_index = 0    
        self.image = self.frames[self.animation_index]
        self.rect = self.image.get_rect(bottomleft = (randint(752, 1000), y_pos))
    
    def animation_state(self):
        self.animation_index += 0.1
        if self.animation_index >= len(self.frames): self.animation_index = 0
        self.image = self.frames[int(self.animation_index)]
       
    def destroy(self):
        if self.rect.x <= -75:
            self.kill()
    
    def update(self):
        self.animation_state()
        self.rect.x -= 5
        self.destroy


def display_score():
    current_time = (pygame.time.get_ticks() - start_time)//2900
    score_surf = test_font.render(f'{current_time}', False, 'lavender')
    score_rect = score_surf.get_rect(center = (400,50))
    screen.blit(score_surf,score_rect)
    
    return current_time


def collision_sprite():
    if pygame.sprite.spritecollide(player.sprite, obstacle_group, False):
        obstacle_group.empty()
        return False
    return True 


pygame.init()
screen = pygame.display.set_mode((680, 367))
pygame.display.set_caption('eggy dont crack')
clock = pygame.time.Clock()
test_font = pygame.font.Font('font/Pixeltype.ttf',50)
game_active = False
start_time = 0
score = 0

# groups
player = pygame.sprite.GroupSingle()
player.add(Player())

obstacle_group = pygame.sprite.Group()


# background importing
sky_surf = pygame.image.load('graphics/sky.jpeg').convert()
ground_surf = pygame.image.load('graphics/ground.png').convert_alpha()

#title screen
player_stand = pygame.image.load('graphics/player/player_stand.png').convert_alpha()
player_stand = pygame.transform.rotozoom(player_stand, 0, 2)
player_stand_rect = player_stand.get_rect(center = (340, 180))

game_name = test_font.render('eggy dont crack', False, 'dark blue')
game_name = pygame.transform.rotozoom(game_name, 0, 1.5)
game_name_rect = game_name.get_rect(center = (340,60))

game_message = test_font.render('press [SPACEBAR] to jump', False, 'cornflower blue')
game_message_rect = game_message.get_rect(center =(340, 320))

#timer
obstacle_timer = pygame.USEREVENT + 1
pygame.time.set_timer(obstacle_timer, 1300)

while True:
    for event in pygame.event.get():
        
        if event.type == pygame.QUIT:
            pygame.quit()
            exit()
        
        # game active/ spawn ememies
        elif game_active and event.type == obstacle_timer:
            obstacle_group.add(Obstacle(choice(['fly','snail','snail'])))
            
        # start new game
        elif (not game_active) and event.type == pygame.KEYDOWN and event.key == pygame.K_SPACE:
                game_active = True
                start_time = pygame.time.get_ticks()
                
                
    if game_active:
        # background painting
        screen.blit(sky_surf, (0,0))
        screen.blit(ground_surf, (0,175))
        
        # draw score
        score = display_score()
        
        # draw player
        player.draw(screen)
        player.update()
        
        # draw obstacles
        obstacle_group.draw(screen)
        obstacle_group.update()
        
        # collisions
        game_active = collision_sprite()
    
    
    # title screen
    else:
        screen.fill('lavender')
        screen.blit(game_name, game_name_rect)
        screen.blit(player_stand,player_stand_rect)
        
        score_message = test_font.render(f'previous score: {score}', False, 'cornflower blue')
        score_message_rect = score_message.get_rect(center=(340, 320))
        
        if score == 0:
            screen.blit(game_message, game_message_rect)
        
        else:
            screen.blit(score_message, score_message_rect)
        
        
    # clock
    pygame.display.update()
    clock.tick(60)
     

