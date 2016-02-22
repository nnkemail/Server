package model

import model.Entities.MovableEntity
import model.Util.util.settings

trait BallPhysics { this: MovableEntity =>
  var _moveEngineTicks = 0
  var _isMoveEngineActive = false
  
  def setMoveEngine(numberOfTicks: Int) = { 
    _isMoveEngineActive = true;
    _moveEngineTicks = numberOfTicks
  }

  def bounce(moveDecay: Double) = {
    // Movement for ejected cells
    var X = position.x + ( speed * Math.sin(moveAngle) );
    var Y = position.y + ( speed * Math.cos(moveAngle) );

 
    speed = speed * moveDecay; 
    
    _moveEngineTicks = _moveEngineTicks - 1;
    if (_moveEngineTicks <= 0)
       _isMoveEngineActive = false
      
  
    var radius = 40;
    if ((position.x - radius) < settings.border.left) {
        moveAngle = 6.28 - moveAngle;
        X = settings.border.left + radius;
    }
    
    if ((position.x + radius) > settings.border.right) {
        moveAngle = 6.28 - moveAngle;
        X = settings.border.right - radius;
    }
    
    if ((position.y - radius) < settings.border.top) {
        moveAngle = if (moveAngle <= 3.14) 
          3.14 - moveAngle 
        else
          9.42 - moveAngle;
        
        Y = settings.border.top + radius;
    }
    
    if ((position.y + radius) > settings.border.bottom) {
        // Flip angle vertically - Bottom side
        moveAngle =  if (moveAngle <= 3.14) 
          3.14 - moveAngle 
        else
          9.42 - moveAngle;
        
        Y = settings.border.bottom - radius;
    }

    // Set position
    position.x = X
    position.y = Y
  }
} 
        
